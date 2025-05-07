// hyggec - The didactic compiler for the Hygge programming language.
// Module for partial evaluation of Hygge AST
module PartialEval

open AST

/// Environment tracking known constant values of variables
type ConstEnv<'E,'T> = Map<string, Node<'E,'T>>

/// Recursively checks if the AST node or any of its subexpressions contains a Pointer
let rec containsPointer (node: Node<'E,'T>) : bool =
    match node.Expr with
    | Pointer(_) -> true
    | Add(lhs, rhs) -> containsPointer lhs || containsPointer rhs
    | Mult(lhs, rhs) -> containsPointer lhs || containsPointer rhs
    | And(lhs, rhs) -> containsPointer lhs || containsPointer rhs
    | Or(lhs, rhs) -> containsPointer lhs || containsPointer rhs
    | Not(arg) -> containsPointer arg
    | Eq(lhs, rhs) -> containsPointer lhs || containsPointer rhs
    | Less(lhs, rhs) -> containsPointer lhs || containsPointer rhs
    | If(cond, ifTrue, ifFalse) -> 
        containsPointer cond || containsPointer ifTrue || containsPointer ifFalse
    | Let(_, init, body) -> containsPointer init || containsPointer body
    | LetT(_, _, init, body) -> containsPointer init || containsPointer body
    | LetMut(_, init, body) -> containsPointer init || containsPointer body
    | Assign(target, expr) -> containsPointer target || containsPointer expr
    | Seq(nodes) -> List.exists containsPointer nodes
    | Application(expr, args) -> 
        containsPointer expr || List.exists containsPointer args
    | Type(_, _, scope) -> containsPointer scope
    | Ascription(_, arg) -> containsPointer arg
    | Assertion(arg) -> containsPointer arg
    | StructCons(fields) -> List.exists (snd >> containsPointer) fields
    | FieldSelect(target, _) -> containsPointer target
    | UnionCons(_, expr) -> containsPointer expr
    | Match(expr, cases) -> 
        containsPointer expr || 
        List.exists (fun (_, _, cont) -> containsPointer cont) cases
    | _ -> false

/// Checks if a node represents a constant value
let isConstant (node: Node<'E,'T>) : bool =
    match node.Expr with
    | UnitVal | BoolVal(_) | IntVal(_) | FloatVal(_) | StringVal(_) -> true
    | _ -> false

/// Optimize AST by applying partial evaluation
let optimize (ast: AST.Node<'E,'T>): AST.Node<'E,'T> =
    Log.info "Applying partial evaluation (constant folding and propagation with pointer handling)"
    
    // Create a special runtime environment for partial evaluation
    // Notice Reader and Printer are None to prevent reducing I/O operations
    let env = { 
        Interpreter.Reader = None
        Interpreter.Printer = None
        Interpreter.Mutables = Map[]
        Interpreter.Heap = Map[]
        Interpreter.PtrInfo = Map[] 
    }
    
    /// Try to apply constant folding using the interpreter while handling pointers
    let rec tryConstantFold (node: AST.Node<'E,'T>) : Option<AST.Node<'E,'T>> =
        // Skip folding for nodes that already represent values
        if Interpreter.isValue node then None
        else
            // First, reduce the node as much as possible and collect all intermediate states
            // This function builds a history from newest (head) to oldest (tail)
            let rec reduceSteps (current: AST.Node<'E,'T>) (history: List<AST.Node<'E,'T>>) =
                match Interpreter.reduce env current with
                | Some(envNew, reduced) ->
                    // Add current reduced node to history and continue reducing
                    reduceSteps reduced (reduced :: history)
                | None ->
                    // Can't reduce further - return history
                    history
            
            // Start with the original node in the history
            let history = reduceSteps node [node]
            
            // Check nodes from newest to oldest (first to last in the list)
            match history |> List.tryFind (fun n -> not (containsPointer n)) with
            | Some node -> Some node  // Found a node without pointers
            | None -> None  // All nodes contain pointers
    
    // Recursive function to optimize a node and its subexpressions
    // with both constant folding and propagation
    and optimizeNode (constEnv: ConstEnv<'E,'T>) (node: AST.Node<'E,'T>) : AST.Node<'E,'T> =
        // Try to apply constant folding first
        match tryConstantFold node with
        | Some result -> result
        | None ->
            // If constant folding is not possible, apply constant propagation
            // and recursively optimize subexpressions
            match node.Expr with
            | Var(name) when constEnv.ContainsKey(name) ->
                constEnv.[name]
                
            | Let(name, init, body) ->
                let init' = optimizeNode constEnv init
                
                // If initializer is a constant, add to environment for body optimization
                let bodyEnv = 
                    if isConstant init' then
                        constEnv.Add(name, init')
                    else
                        constEnv
                        
                let body' = optimizeNode bodyEnv body
                
                // If we're binding to a constant and it's used in the body,
                // we might be able to optimize further with the consolidated AST
                let consolidated = {node with Expr = Let(name, init', body')}
                match tryConstantFold consolidated with
                | Some result -> result
                | None -> consolidated
            
            | LetT(name, tpe, init, body) ->
                let init' = optimizeNode constEnv init
                
                let bodyEnv = 
                    if isConstant init' then
                        constEnv.Add(name, init')
                    else
                        constEnv
                        
                let body' = optimizeNode bodyEnv body
                {node with Expr = LetT(name, tpe, init', body')}
            
            | LetMut(name, init, body) ->
                let init' = optimizeNode constEnv init
                let body' = optimizeNode constEnv body  // Don't propagate mutable vars
                {node with Expr = LetMut(name, init', body')}
            
            | Assign({Expr = Var(name)} as target, expr) ->
                let expr' = optimizeNode constEnv expr
                {node with Expr = Assign(target, expr')}
            
            | Add(lhs, rhs) ->
                let lhs' = optimizeNode constEnv lhs
                let rhs' = optimizeNode constEnv rhs
                
                // Try constant folding after propagation
                match (lhs'.Expr, rhs'.Expr) with
                | (IntVal(v1), IntVal(v2)) -> {node with Expr = IntVal(v1 + v2)}
                | (FloatVal(v1), FloatVal(v2)) -> {node with Expr = FloatVal(v1 + v2)}
                | _ -> {node with Expr = Add(lhs', rhs')}
            
            | Mult(lhs, rhs) ->
                let lhs' = optimizeNode constEnv lhs
                let rhs' = optimizeNode constEnv rhs
                
                match (lhs'.Expr, rhs'.Expr) with
                | (IntVal(v1), IntVal(v2)) -> {node with Expr = IntVal(v1 * v2)}
                | (FloatVal(v1), FloatVal(v2)) -> {node with Expr = FloatVal(v1 * v2)}
                | _ -> {node with Expr = Mult(lhs', rhs')}
                
            | And(lhs, rhs) ->
                let lhs' = optimizeNode constEnv lhs
                let rhs' = optimizeNode constEnv rhs
                
                match (lhs'.Expr, rhs'.Expr) with
                | (BoolVal(v1), BoolVal(v2)) -> {node with Expr = BoolVal(v1 && v2)}
                | _ -> {node with Expr = And(lhs', rhs')}
                
            | Or(lhs, rhs) ->
                let lhs' = optimizeNode constEnv lhs
                let rhs' = optimizeNode constEnv rhs
                
                match (lhs'.Expr, rhs'.Expr) with
                | (BoolVal(v1), BoolVal(v2)) -> {node with Expr = BoolVal(v1 || v2)}
                | _ -> {node with Expr = Or(lhs', rhs')}
                
            | Not(arg) ->
                let arg' = optimizeNode constEnv arg
                
                match arg'.Expr with
                | BoolVal(v) -> {node with Expr = BoolVal(not v)}
                | _ -> {node with Expr = Not(arg')}
                
            | Eq(lhs, rhs) ->
                let lhs' = optimizeNode constEnv lhs
                let rhs' = optimizeNode constEnv rhs
                
                match (lhs'.Expr, rhs'.Expr) with
                | (IntVal(v1), IntVal(v2)) -> {node with Expr = BoolVal(v1 = v2)}
                | (FloatVal(v1), FloatVal(v2)) -> {node with Expr = BoolVal(v1 = v2)}
                | (BoolVal(v1), BoolVal(v2)) -> {node with Expr = BoolVal(v1 = v2)}
                | (StringVal(v1), StringVal(v2)) -> {node with Expr = BoolVal(v1 = v2)}
                | _ -> {node with Expr = Eq(lhs', rhs')}
                
            | Less(lhs, rhs) ->
                let lhs' = optimizeNode constEnv lhs
                let rhs' = optimizeNode constEnv rhs
                
                match (lhs'.Expr, rhs'.Expr) with
                | (IntVal(v1), IntVal(v2)) -> {node with Expr = BoolVal(v1 < v2)}
                | (FloatVal(v1), FloatVal(v2)) -> {node with Expr = BoolVal(v1 < v2)}
                | _ -> {node with Expr = Less(lhs', rhs')}
                
            | If(cond, ifTrue, ifFalse) ->
                let cond' = optimizeNode constEnv cond
                
                // Dead code elimination for constant conditions
                match cond'.Expr with
                | BoolVal(true) -> optimizeNode constEnv ifTrue
                | BoolVal(false) -> optimizeNode constEnv ifFalse
                | _ -> 
                    let ifTrue' = optimizeNode constEnv ifTrue
                    let ifFalse' = optimizeNode constEnv ifFalse
                    {node with Expr = If(cond', ifTrue', ifFalse')}
                    
            | Seq(nodes) ->
                let nodes' = List.map (optimizeNode constEnv) nodes
                match nodes' with
                | [] -> {node with Expr = UnitVal}
                | [last] when isConstant last -> last
                | _ -> {node with Expr = Seq(nodes')}
                
            | Application(expr, args) ->
                let expr' = optimizeNode constEnv expr
                let args' = List.map (optimizeNode constEnv) args
                
                // Try to inline function calls if lambda is known
                match expr'.Expr with
                | Lambda(lamArgs, body) when List.forall isConstant args' && 
                                             args'.Length = lamArgs.Length ->
                    // All arguments are constant and match parameter count - can inline
                    let (lamArgNames, _) = List.unzip lamArgs
                    let lamArgNamesValues = List.zip lamArgNames args'
                    let folder acc (var, sub) = (ASTUtil.subst acc var sub)
                    let inlinedBody = List.fold folder body lamArgNamesValues
                    optimizeNode constEnv inlinedBody  // Optimize the inlined body
                | _ ->
                    {node with Expr = Application(expr', args')}
            
            | Type(name, tpe, scope) ->
                let scope' = optimizeNode constEnv scope
                {node with Expr = Type(name, tpe, scope')}
                
            | Ascription(tpe, arg) ->
                let arg' = optimizeNode constEnv arg
                {node with Expr = Ascription(tpe, arg')}
                
            | Assertion(arg) ->
                let arg' = optimizeNode constEnv arg
                
                match arg'.Expr with
                | BoolVal(true) -> {node with Expr = UnitVal}
                | _ -> {node with Expr = Assertion(arg')}
                
            // For all other node types, keep them as is
            | _ -> node
    
    /// Try to apply constant folding using the interpreter while handling pointers
    let tryConstantFold (node: AST.Node<'E,'T>) : Option<AST.Node<'E,'T>> =
        // Skip folding for nodes that already represent values
        if Interpreter.isValue node then None
        else
            // First, reduce the node as much as possible and collect all intermediate states
            // This function builds a history from newest (head) to oldest (tail)
            let rec reduceSteps (current: AST.Node<'E,'T>) (history: List<AST.Node<'E,'T>>) =
                match Interpreter.reduce env current with
                | Some(_, reduced) ->
                    // Add current reduced node to history and continue reducing
                    reduceSteps reduced (reduced :: history)
                | None ->
                    // Can't reduce further - return history
                    history
            
            // Start with the original node in the history
            let history = reduceSteps node [node]
            
            // Check nodes from newest to oldest (first to last in the list)
            match history |> List.tryFind (fun n -> not (containsPointer n)) with
            | Some node -> Some node  // Found a node without pointers
            | None -> None  // All nodes contain pointers
    
    // Start optimization from the root node with empty constant environment
    optimizeNode Map.empty ast
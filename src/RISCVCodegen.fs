// hyggec - The didactic compiler for the Hygge programming language.
// Copyright (C) 2023 Technical University of Denmark
// Author: Alceste Scalas <alcsc@dtu.dk>
// Released under the MIT license (see LICENSE.md for details)

/// Functions to generate RISC-V assembly code from a typed Hygge AST.
module RISCVCodegen

open AST
open RISCV
open Type
open Typechecker


/// Exit code used in the generated assembly to signal an assertion violation.
let assertExitCode = 42 // Must be non-zero


/// Storage information for variables.
[<RequireQualifiedAccess; StructuralComparison; StructuralEquality>]
type internal Storage =
    /// The variable is stored in an integerregister.
    | Reg of reg: Reg
    /// The variable is stored in a floating-point register.
    | FPReg of fpreg: FPReg
    /// The variable is stored in memory, in a location marked with a
    /// label in the compiled assembly code.
    | Label of label: string
    /// This variable is stored on the stack, at the given offset (in bytes)
    /// from the memory address contained in the frame pointer (fp) register.
    | Frame of offset: int


/// Code generation environment.
type internal CodegenEnv = {
    /// Target register number for the result of non-floating-point expressions.
    Target: uint
    /// Target register number for the result of floating-point expressions.
    FPTarget: uint
    /// Storage information about known variables.
    VarStorage: Map<string, Storage>
}


/// Code generation function: compile the expression in the given AST node so
/// that it writes its results on the 'Target' and 'FPTarget' generic register
/// numbers (specified in the given codegen 'env'ironment).  IMPORTANT: the
/// generated code must never modify the contents of register numbers lower than
/// the given targets.
let rec internal doCodegen (env: CodegenEnv) (node: TypedAST): Asm =
    match node.Expr with
    | UnitVal -> Asm() // Nothing to do

    | BoolVal(v) ->
        /// Boolean constant turned into integer 1 if true, or 0 if false
        let value = if v then 1 else 0
        Asm(RV.LI(Reg.r(env.Target), value), $"Bool value '%O{v}'")

    | IntVal(v) ->
        Asm(RV.LI(Reg.r(env.Target), v))

    | FloatVal(v) ->
        // We convert the float value into its bytes, and load it as immediate
        let bytes = System.BitConverter.GetBytes(v)
        if (not System.BitConverter.IsLittleEndian)
            then System.Array.Reverse(bytes) // RISC-V is little-endian
        let word: int32 = System.BitConverter.ToInt32(bytes)
        Asm([ (RV.LI(Reg.r(env.Target), word), $"Float value %f{v}")
              (RV.FMV_W_X(FPReg.r(env.FPTarget), Reg.r(env.Target)), "") ])

    | StringVal(v) ->
        // Label marking the string constant in the data segment
        let label = Util.genSymbol "string_val"
        Asm().AddData(label, Alloc.String(v))
             .AddText(RV.LA(Reg.r(env.Target), label))


    | Var(name) ->
        // To compile a variable, we inspect its type and where it is stored
        match node.Type with
        | t when (isSubtypeOf node.Env t TUnit)
            -> Asm() // A unit-typed variable is just ignored
        | t when (isSubtypeOf node.Env t TFloat) ->
            match (env.VarStorage.TryFind name) with
            | Some(Storage.FPReg(fpreg)) ->
                Asm(RV.FMV_S(FPReg.r(env.FPTarget), fpreg),
                    $"Load variable '%s{name}'")
            | Some(Storage.Label(lab)) ->
                Asm([ (RV.LA(Reg.r(env.Target), lab),
                       $"Load address of variable '%s{name}'")
                      (RV.LW(Reg.r(env.Target), Imm12(0), Reg.r(env.Target)),
                       $"Load value of variable '%s{name}'")
                      (RV.FMV_W_X(FPReg.r(env.FPTarget), Reg.r(env.Target)),
                       $"Transfer '%s{name}' to fp register") ])
            | Some(Storage.Frame(offset)) ->
                // Load a float variable from the stack at fp+offset
                Asm([ (RV.LW(Reg.r(env.Target), Imm12(offset), Reg.fp),
                       $"Load stack variable '%s{name}' from fp+%d{offset}")
                      (RV.FMV_W_X(FPReg.r(env.FPTarget), Reg.r(env.Target)),
                       $"Transfer '%s{name}' to fp register") ])
            | Some(Storage.Reg(_)) as st ->
                failwith $"BUG: variable %s{name} of type %O{t} has unexpected storage %O{st}"
            | None -> failwith $"BUG: float variable without storage: %s{name}"
        | t ->  // Default case for variables holding integer-like values
            match (env.VarStorage.TryFind name) with
            | Some(Storage.Reg(reg)) ->
                Asm(RV.MV(Reg.r(env.Target), reg), $"Load variable '%s{name}'")
            | Some(Storage.Label(lab)) ->
                match (expandType node.Env node.Type) with
                    | TFun(_,_) ->
                        Asm(RV.LA(Reg.r(env.Target), lab),
                            $"Load variable '%s{name}' (labmda term)")
                    | _ ->
                        Asm([ (RV.LA(Reg.r(env.Target), lab),
                               $"Load address of variable '%s{name}'")
                              (RV.LW(Reg.r(env.Target), Imm12(0), Reg.r(env.Target)),
                               $"Load value of variable '%s{name}'") ])
            | Some(Storage.Frame(offset)) ->
                // Load a variable from the stack at fp+offset
                Asm(RV.LW(Reg.r(env.Target), Imm12(offset), Reg.fp),
            $"Load stack variable '%s{name}' from fp+%d{offset}")
            | Some(Storage.FPReg(_)) as st ->
                failwith $"BUG: variable %s{name} of type %O{t} has unexpected storage %O{st}"
            | None -> failwith $"BUG: variable without storage: %s{name}"
    | Match(_, _) ->
        failwith "Match expressions not yet supported"

    | Add(lhs, rhs)
    | Mult(lhs, rhs) as expr ->
        // Code generation for addition and multiplication is very
        // similar: we compile the lhs and rhs giving them different target
        // registers, and then apply the relevant assembly operation(s) on their
        // results.

        /// Generated code for the lhs expression
        let lAsm = doCodegen env lhs
        // The generated code depends on the type of addition being computed
        match node.Type with
        | t when (isSubtypeOf node.Env t TInt) ->
            /// Target register for the rhs expression
            let rtarget = env.Target + 1u
            /// Generated code for the rhs expression
            let rAsm = doCodegen {env with Target = rtarget} rhs
            /// Generated code for the numerical operation
            let opAsm =
                match expr with
                    | Add(_,_) ->
                        Asm(RV.ADD(Reg.r(env.Target),
                                   Reg.r(env.Target), Reg.r(rtarget)))
                    | Mult(_,_) ->
                        Asm(RV.MUL(Reg.r(env.Target),
                                   Reg.r(env.Target), Reg.r(rtarget)))
                    | x -> failwith $"BUG: unexpected operation %O{x}"
            // Put everything together
            lAsm ++ rAsm ++ opAsm
        | t when (isSubtypeOf node.Env t TFloat) ->
            /// Target register for the rhs expression
            let rfptarget = env.FPTarget + 1u
            /// Generated code for the rhs expression
            let rAsm = doCodegen {env with FPTarget = rfptarget} rhs
            /// Generated code for the numerical operation
            let opAsm =
                match expr with
                | Add(_,_) ->
                    Asm(RV.FADD_S(FPReg.r(env.FPTarget),
                                  FPReg.r(env.FPTarget), FPReg.r(rfptarget)))
                | Mult(_,_) ->
                    Asm(RV.FMUL_S(FPReg.r(env.FPTarget),
                                  FPReg.r(env.FPTarget), FPReg.r(rfptarget)))
                | x -> failwith $"BUG: unexpected operation %O{x}"
            // Put everything together
            lAsm ++ rAsm ++ opAsm
        | t ->
            failwith $"BUG: numerical operation codegen invoked on invalid type %O{t}"

    | And(lhs, rhs)
    | Or(lhs, rhs) as expr ->
        // Code generation for logical 'and' and 'or' is very similar: we
        // compile the lhs and rhs giving them different target registers, and
        // then apply the relevant assembly operation(s) on their results.
        
        /// Generated code for the lhs expression
        let lAsm = doCodegen env lhs
        /// Target register for the rhs expression
        let rtarget = env.Target + 1u
        /// Generated code for the rhs expression
        let rAsm = doCodegen {env with Target = rtarget} rhs
        /// Generated code for the logical operation
        let opAsm =
            match expr with
            | And(_,_) ->
                Asm(RV.AND(Reg.r(env.Target), Reg.r(env.Target), Reg.r(rtarget)))
            | Or(_,_) ->
                Asm(RV.OR(Reg.r(env.Target), Reg.r(env.Target), Reg.r(rtarget)))
            | x -> failwith $"BUG: unexpected operation %O{x}"
        // Put everything together
        lAsm ++ rAsm ++ opAsm

    | Not(arg) ->
        /// Generated code for the argument expression (note that we don't need
        /// to increase its target register)
        let asm = doCodegen env arg
        asm.AddText(RV.SEQZ(Reg.r(env.Target), Reg.r(env.Target)))

//copy
    | DeepCopy(arg) ->
        // Generate code to compute the argument (get the struct address)
        let argCode = doCodegen env arg
        // Get struct type information
        match (expandType arg.Env arg.Type) with
        | TStruct(fields) ->
            // Helper function to implement recursive deep copy
            let rec generateDeepCopyCode (sourceReg: uint) (targetReg: uint) (structType: Type) : Asm =
                match (expandType arg.Env structType) with
                | TStruct(nestedFields) ->
                    let fieldCount = nestedFields.Length
                    // Allocate memory for the new struct
                    let allocCode =
                        (beforeSysCall [Reg.a0] [])
                            .AddText([
                                (RV.LI(Reg.a0, fieldCount * 4),
                                "Allocate memory for struct copy (in bytes)")
                                (RV.LI(Reg.a7, 9), "RARS syscall: Sbrk")
                                (RV.ECALL, "")
                                (RV.MV(Reg.r(targetReg), Reg.a0),
                                "Store new struct address")
                            ])
                            ++ (afterSysCall [Reg.a0] [])
                    // Generate code to copy each field
                    let copyFieldsCode = 
                        let mutable code = Asm()
                        for i in 0 .. (fieldCount - 1) do
                            let (fieldName, fieldType) = nestedFields.[i]
                            if isSubtypeOf arg.Env fieldType TFloat then
                                // Copy float field
                                code <- code.AddText([
                                    (RV.FLW_S(FPReg.r(env.FPTarget), Imm12(i * 4), Reg.r(sourceReg)),
                                    $"Load field {fieldName} from source struct (float)")
                                    (RV.FSW_S(FPReg.r(env.FPTarget), Imm12(i * 4), Reg.r(targetReg)),
                                    $"Copy field {fieldName} to target struct (float)")
                                ])
                            else if not (isSubtypeOf arg.Env fieldType TUnit) then
                                match (expandType arg.Env fieldType) with
                                | TStruct(_) ->
                                    // Handle nested structs
                                    let nestedSourceReg = sourceReg + 2u
                                    let nestedTargetReg = targetReg + 3u
                                    // Load pointer to nested struct
                                    code <- code.AddText([
                                        (RV.LW(Reg.r(nestedSourceReg), Imm12(i * 4), Reg.r(sourceReg)),
                                        $"Load nested struct pointer from field {fieldName}")
                                    ])
                                    // Save current register state
                                    code <- code.AddText([
                                        (RV.SW(Reg.r(sourceReg), Imm12(-4), Reg.sp), "Save source struct address")
                                        (RV.SW(Reg.r(targetReg), Imm12(-8), Reg.sp), "Save target struct address")
                                        (RV.ADDI(Reg.sp, Reg.sp, Imm12(-8)), "Adjust stack pointer")
                                    ])
                                    // Recursively copy nested struct
                                    code <- code ++ generateDeepCopyCode nestedSourceReg nestedTargetReg fieldType
                                    // Restore register state
                                    code <- code.AddText([
                                        (RV.ADDI(Reg.sp, Reg.sp, Imm12(8)), "Restore stack pointer")
                                        (RV.LW(Reg.r(sourceReg), Imm12(-4), Reg.sp), "Restore source struct address")
                                        (RV.LW(Reg.r(targetReg), Imm12(-8), Reg.sp), "Restore target struct address")
                                    ])
                                    // Store new nested struct address in current struct
                                    code <- code.AddText([
                                        (RV.SW(Reg.r(nestedTargetReg), Imm12(i * 4), Reg.r(targetReg)),
                                        $"Store new nested struct address to field {fieldName}")
                                    ])
                                | _ ->
                                    // Copy regular field
                                    code <- code.AddText([
                                        (RV.LW(Reg.r(sourceReg + 2u), Imm12(i * 4), Reg.r(sourceReg)),
                                        $"Load field {fieldName} from source struct")
                                        (RV.SW(Reg.r(sourceReg + 2u), Imm12(i * 4), Reg.r(targetReg)),
                                        $"Copy field {fieldName} to target struct")
                                    ])
                        code
                    // Move result to target register
                    let moveResultCode = 
                        Asm(RV.MV(Reg.r(env.Target), Reg.r(targetReg)),
                            "Move final struct address to target register")
                    allocCode ++ copyFieldsCode ++ moveResultCode
                | _ -> 
                    Asm() // Not a struct type
            // Call recursive function to generate deep copy code
            argCode ++ generateDeepCopyCode env.Target (env.Target + 1u) arg.Type
        | t ->
            failwith $"Error: Copy operation can only be applied to struct types, not: {t}"

    | ShallowCopy(arg) ->
        let argCode = doCodegen env arg
        // Get struct type information to determine field count and layout
        match (expandType arg.Env arg.Type) with
        | TStruct(fields) ->
            let fieldCount = fields.Length
        // Step 1: Allocate new heap memory space for the copied structure
            let structAllocCode =
                (beforeSysCall [Reg.a0] [])
                    .AddText([
                        (RV.LI(Reg.a0, fieldCount * 4),
                        "Allocate memory space for the struct copy (in bytes)")
                        (RV.LI(Reg.a7, 9), "RARS syscall: Sbrk")
                        (RV.ECALL, "")
                        (RV.MV(Reg.r(env.Target + 1u), Reg.a0),
                        "Save system call result (struct memory address) to temporary register")
                    ])
                    ++ (afterSysCall [Reg.a0] [])
        
        // Step 2: Generate copy code for each field
            let copyFieldsCode = 
            // Use a loop to generate code to copy each field
                let mutable copyCode = Asm()
                for i in 0 .. (fieldCount - 1) do
                    let fieldType = snd fields.[i]
                    if isSubtypeOf arg.Env fieldType TFloat then
                    // Floating point fields
                        copyCode <- copyCode.AddText([
                            (RV.FLW_S(FPReg.r(env.FPTarget), Imm12(i * 4), Reg.r(env.Target)),
                            $"Load field %d{i} from source struct (floating point)")
                            (RV.FSW_S(FPReg.r(env.FPTarget), Imm12(i * 4), Reg.r(env.Target + 1u)),
                            $"Copy field %d{i} to target struct (floating point)")
                    ])
                    else if not (isSubtypeOf arg.Env fieldType TUnit) then
                    // Integer and other type fields
                        copyCode <- copyCode.AddText([
                            (RV.LW(Reg.r(env.Target + 2u), Imm12(i * 4), Reg.r(env.Target)),
                            $"Load field %d{i} from source struct")
                            (RV.SW(Reg.r(env.Target + 2u), Imm12(i * 4), Reg.r(env.Target + 1u)),
                            $"Copy field %d{i} to target struct")
                        ])
                copyCode
        
        // Final step: Move target struct address to target register
            let finalCode = 
                Asm(RV.MV(Reg.r(env.Target), Reg.r(env.Target + 1u)),
                    "Move copied struct address to target register")
        
        // Combine all code together
            argCode ++ structAllocCode ++ copyFieldsCode ++ finalCode
        
        | t ->
            failwith $"Error: Copy operation can only be used on struct types, not: %O{t}"

    | Eq(lhs, rhs)
    | Less(lhs, rhs) as expr ->
        // Code generation for equality and less-than relations is very similar:
        // we compile the lhs and rhs giving them different target registers,
        // and then apply the relevant assembly operation(s) on their results.

        /// Generated code for the lhs expression
        let lAsm = doCodegen env lhs
        // The generated code depends on the lhs and rhs types
        match lhs.Type with
        | t when (isSubtypeOf lhs.Env t TInt) ->
            // Our goal is to write 1 (true) or 0 (false) in the register
            // env.Target, depending on the result of the comparison between
            // the lhs and rhs.  To achieve this, we perform a conditional
            // branch depending on whether the lhs and rhs are equal (or the lhs
            // is less than the rhs):
            // - if the comparison is true, we jump to a label where we write
            //   1 in the target register, and continue
            // - if the comparison is false, we write 0 in the target register
            //   and we jump to a label marking the end of the generated code

            /// Target register for the rhs expression
            let rtarget = env.Target + 1u
            /// Generated code for the rhs expression
            let rAsm = doCodegen {env with Target = rtarget} rhs
            
            /// Human-readable prefix for jump labels, describing the kind of
            /// relational operation we are compiling
            let labelName = match expr with
                            | Eq(_,_) -> "eq"
                            | Less(_,_) -> "less"
                            | x -> failwith $"BUG: unexpected operation %O{x}"
            /// Label to jump to when the comparison is true
            let trueLabel = Util.genSymbol $"%O{labelName}_true"
            /// Label to mark the end of the comparison code
            let endLabel = Util.genSymbol $"%O{labelName}_end"

            /// Codegen for the relational operation between lhs and rhs
            let opAsm =
                match expr with
                | Eq(_,_) ->
                    Asm(RV.BEQ(Reg.r(env.Target), Reg.r(rtarget), trueLabel))
                | Less(_,_) ->
                    Asm(RV.BLT(Reg.r(env.Target), Reg.r(rtarget), trueLabel))
                | x -> failwith $"BUG: unexpected operation %O{x}"

            // Put everything together
            (lAsm ++ rAsm ++ opAsm)
                .AddText([
                    (RV.LI(Reg.r(env.Target), 0), "Comparison result is false")
                    (RV.J(endLabel), "")
                    (RV.LABEL(trueLabel), "")
                    (RV.LI(Reg.r(env.Target), 1), "Comparison result is true")
                    (RV.LABEL(endLabel), "")
                ])
        | t when (isSubtypeOf lhs.Env t TFloat) ->
            /// Target register for the rhs expression
            let rfptarget = env.FPTarget + 1u
            /// Generated code for the rhs expression
            let rAsm = doCodegen {env with FPTarget = rfptarget} rhs
            /// Generated code for the relational operation
            let opAsm =
                match expr with
                | Eq(_,_) ->
                    Asm(RV.FEQ_S(Reg.r(env.Target), FPReg.r(env.FPTarget), FPReg.r(rfptarget)))
                | Less(_,_) ->
                    Asm(RV.FLT_S(Reg.r(env.Target), FPReg.r(env.FPTarget), FPReg.r(rfptarget)))
                | x -> failwith $"BUG: unexpected operation %O{x}"
            // Put everything together
            (lAsm ++ rAsm ++ opAsm)
        | t ->
            failwith $"BUG: relational operation codegen invoked on invalid type %O{t}"

    | ReadInt ->
        (beforeSysCall [Reg.a0] [])
            .AddText([
                (RV.LI(Reg.a7, 5), "RARS syscall: ReadInt")
                (RV.ECALL, "")
                (RV.MV(Reg.r(env.Target), Reg.a0), "Move syscall result to target")
            ])
            ++ (afterSysCall [Reg.a0] [])

    | ReadFloat ->
        (beforeSysCall [] [FPReg.fa0])
            .AddText([
                (RV.LI(Reg.a7, 6), "RARS syscall: ReadFloat")
                (RV.ECALL, "")
                (RV.FMV_S(FPReg.r(env.FPTarget), FPReg.fa0), "Move syscall result to target")
            ])
            ++ (afterSysCall [] [FPReg.fa0])

    | Print(arg) ->
        /// Compiled code for the 'print' argument, leaving its result on the
        /// generic register 'target' or 'fptarget' (depending on its type)
        let argCode = doCodegen env arg
        // The generated code depends on the 'print' argument type
        match arg.Type with
        | t when (isSubtypeOf arg.Env t TBool) ->
            let strTrue = Util.genSymbol "true"
            let strFalse = Util.genSymbol "false"
            let printFalse = Util.genSymbol "print_true"
            let printExec = Util.genSymbol "print_execute"
            argCode.AddData(strTrue, Alloc.String("true"))
                .AddData(strFalse, Alloc.String("false"))
                ++ (beforeSysCall [Reg.a0] [])
                  .AddText([
                    (RV.BEQZ(Reg.r(env.Target), printFalse), "")
                    (RV.LA(Reg.a0, strTrue), "String to print via syscall")
                    (RV.J(printExec), "")
                    (RV.LABEL(printFalse), "")
                    (RV.LA(Reg.a0, strFalse), "String to print via syscall")
                    (RV.LABEL(printExec), "")
                    (RV.LI(Reg.a7, 4), "RARS syscall: PrintString")
                    (RV.ECALL, "")
                  ])
                  ++ (afterSysCall [Reg.a0] [])
        | t when (isSubtypeOf arg.Env t TInt) ->
            argCode
            ++ (beforeSysCall [Reg.a0] [])
                .AddText([
                    (RV.MV(Reg.a0, Reg.r(env.Target)), "Copy to a0 for printing")
                    (RV.LI(Reg.a7, 1), "RARS syscall: PrintInt")
                    (RV.ECALL, "")
                ])
                ++ (afterSysCall [Reg.a0] [])
        | t when (isSubtypeOf arg.Env t TFloat) ->
            argCode
            ++ (beforeSysCall [] [FPReg.fa0])
                .AddText([
                    (RV.FMV_S(FPReg.fa0, FPReg.r(env.FPTarget)), "Copy to fa0 for printing")
                    (RV.LI(Reg.a7, 2), "RARS syscall: PrintFloat")
                    (RV.ECALL, "")
                ])
                ++ (afterSysCall [] [FPReg.fa0])
        | t when (isSubtypeOf arg.Env t TString) ->
            argCode
            ++ (beforeSysCall [Reg.a0] [])
                .AddText([
                    (RV.MV(Reg.a0, Reg.r(env.Target)), "Copy to a0 for printing")
                    (RV.LI(Reg.a7, 4), "RARS syscall: PrintString")
                    (RV.ECALL, "")
                ])
                ++ (afterSysCall [Reg.a0] [])
        | t ->
            failwith $"BUG: Print codegen invoked on unsupported type %O{t}"

    | PrintLn(arg) ->
        // Recycle codegen for Print above, then also output a newline
        (doCodegen env {node with Expr = Print(arg)})
        ++ (beforeSysCall [Reg.a0] [])
            .AddText([
                (RV.LI(Reg.a7, 11), "RARS syscall: PrintChar")
                (RV.LI(Reg.a0, int('\n')), "Character to print (newline)")
                (RV.ECALL, "")
            ])
            ++ (afterSysCall [Reg.a0] [])

    | If(condition, ifTrue, ifFalse) ->
        /// Label to jump to when the 'if' condition is true
        let labelTrue = Util.genSymbol "if_true"
        /// Label to jump to when the 'if' condition is false
        let labelFalse = Util.genSymbol "if_false"
        /// Label to mark the end of the if..then...else code
        let labelEnd = Util.genSymbol "if_end"
        // Compile the 'if' condition; if the result is true (i.e., not zero)
        // then jump to 'labelTrue', execute the 'ifTrue' code, and finally jump
        // to 'labelEnd' (thus skipping the code under 'labelFalse'). Otherwise
        // (i.e., when the 'if' condition result is false) jump to 'labelFalse'
        // and execute the 'ifFalse' code. Here we use a register to load the
        // address of a label (using the instruction LA) and then jump to it
        // (using the instruction JR): this way, the label address can be very
        // far from the jump instruction address --- and this can be important
        // if the compilation of 'ifTrue' and/or 'ifFalse' produces a large
        // amount of assembly code
        (doCodegen env condition)
            .AddText([ (RV.BNEZ(Reg.r(env.Target), labelTrue),
                        "Jump when 'if' condition is true")
                       (RV.LA(Reg.r(env.Target), labelFalse),
                        "Load the address of the 'false' branch of the 'if' code")
                       (RV.JR(Reg.r(env.Target)),
                        "Jump to the 'false' branch of the 'if' code")
                       (RV.LABEL(labelTrue),
                        "Beginning of the 'true' branch of the 'if' code") ])
            ++ (doCodegen env ifTrue)
                .AddText([ (RV.LA(Reg.r(env.Target + 1u), labelEnd),
                            "Load the address of the end of the 'if' code")
                           (RV.JR(Reg.r(env.Target + 1u)),
                            "Jump to skip the 'false' branch of 'if' code")
                           (RV.LABEL(labelFalse),
                            "Beginning of the 'false' branch of the 'if' code") ])
                ++ (doCodegen env ifFalse)
                    .AddText(RV.LABEL(labelEnd), "End of the 'if' code")

    | Seq(nodes) ->
        // We collect the code of each sequence node by folding over all nodes
        let folder (asm: Asm) (node: TypedAST) =
            asm ++ (doCodegen env node)
        List.fold folder (Asm()) nodes

    | Type(_, _, scope) ->
        // A type alias does not produce any code --- but its scope does
        doCodegen env scope

    | Ascription(_, node) ->
        // A type ascription does not produce code --- but the type-annotated
        // AST node does
        doCodegen env node

    | Assertion(arg) ->
        /// Label to jump to when the assertion is true
        let passLabel = Util.genSymbol "assert_true"
        // Check the assertion, and jump to 'passLabel' if it is true;
        // otherwise, fail
        (doCodegen env arg)
            .AddText([
                (RV.ADDI(Reg.r(env.Target), Reg.r(env.Target), Imm12(-1)), "")
                (RV.BEQZ(Reg.r(env.Target), passLabel), "Jump if assertion OK")
                (RV.LI(Reg.a7, 93), "RARS syscall: Exit2")
                (RV.LI(Reg.a0, assertExitCode), "Assertion violation exit code")
                (RV.ECALL, "")
                (RV.LABEL(passLabel), "")
            ])

    // Special case for compiling a function with a given immutable name in the
    // input source file.  We recognise this case by checking whether the
    // 'Let...' declares 'name' as a Lambda expression with a TFun type
    | Let(name, {Node.Expr = Lambda(args, body);
                 Node.Type = TFun(targs, _)}, scope)
    | LetT(name, _, {Node.Expr = Lambda(args, body);
                     Node.Type = TFun(targs, _)}, scope) ->
        /// Assembly label to mark the position of the compiled function body.
        /// For readability, we make the label similar to the function name
        let funLabel = Util.genSymbol $"fun_%s{name}"

        /// Names of the lambda term arguments
        let (argNames, _) = List.unzip args
        /// List of pairs associating each function argument to its type
        let argNamesTypes = List.zip argNames targs
        /// Compiled function body
        let bodyCode = compileFunction argNamesTypes body env
        
        /// Compiled function code where the function label is located just
        /// before the 'bodyCode', and everything is placed at the end of the
        /// Text segment (i.e. in the "PostText")
        let funCode =
            (Asm(RV.LABEL(funLabel), $"Code for function '%s{name}'")
                ++ bodyCode).TextToPostText

        /// Storage info where the name of the compiled function points to the
        /// label 'funLabel'
        let varStorage2 = env.VarStorage.Add(name, Storage.Label(funLabel))

        // Finally, compile the 'let...'' scope with the newly-defined function
        // label in the variables storage, and append the 'funCode' above. The
        // 'scope' code leaves its result in the the 'let...' target register
        (doCodegen {env with VarStorage = varStorage2} scope)
            ++ funCode

    // Special case for compiling a recursive function with a given immutable name
    // We recognize this case by checking whether the 'LetRec' declares 'name' as a Lambda expression with a TFun type
    | LetRec(name, _, {Node.Expr = Lambda(args, body); 
                       Node.Type = TFun(targs, _)}, scope) ->
        /// Assembly label to mark the position of the compiled function body.
        /// For readability, we make the label similar to the function name
        let funLabel = Util.genSymbol $"fun_%s{name}"

        /// Names of the lambda term arguments
        let (argNames, _) = List.unzip args
        /// List of pairs associating each function argument to its type
        let argNamesTypes = List.zip argNames targs
    
        /// Storage info where the name of the compiled function points to the
        /// label 'funLabel' - This is critical for recursion to work
        let varStorage2 = env.VarStorage.Add(name, Storage.Label(funLabel))
    
        /// Compiled function body - using the updated environment that includes the function itself
        let bodyCode = compileFunction argNamesTypes body {env with VarStorage = varStorage2}
    
        /// Compiled function code where the function label is located just
        /// before the 'bodyCode', and everything is placed at the end of the
        /// Text segment (i.e. in the "PostText")
        let funCode =
            (Asm(RV.LABEL(funLabel), $"Code for recursive function '%s{name}'")
                ++ bodyCode).TextToPostText

        // Finally, compile the 'let rec' scope with the newly-defined function
        // label in the variables storage, and append the 'funCode' above.
        (doCodegen {env with VarStorage = varStorage2} scope)
            ++ funCode

    | Let(name, init, scope)
    | LetT(name, _, init, scope)
    | LetMut(name, init, scope) ->
        /// 'let...' initialisation code, which leaves its result in the
        /// 'target' register (which we overwrite at the end of the 'scope'
        /// execution)
        let initCode = doCodegen env init
        match init.Type with
        | t when (isSubtypeOf init.Env t TUnit) ->
        // The 'init' produces a unit value, i.e. nothing: we can keep using
        // the same target registers, and we don't need to update the
        // variables-to-registers mapping.
            initCode ++ (doCodegen env scope)
        | t when (isSubtypeOf init.Env t TFloat) ->
        /// Target register for compiling the 'let' scope
            let scopeTarget = env.FPTarget + 1u
        /// Variable storage for compiling the 'let' scope
            let scopeVarStorage =
                env.VarStorage.Add(name, Storage.FPReg(FPReg.r(env.FPTarget)))
        /// Environment for compiling the 'let' scope
            let scopeEnv = { env with FPTarget = scopeTarget
                                      VarStorage = scopeVarStorage }
            initCode
                ++ (doCodegen scopeEnv scope)
                    .AddText(RV.FMV_S(FPReg.r(env.FPTarget),
                                      FPReg.r(scopeTarget)),
                            "Move result of 'let' scope expression into target register")
        | _ ->  // Default case for integer-like initialisation expressions
            /// Target register for compiling the 'let' scope
            let scopeTarget = env.Target + 1u
            /// Variable storage for compiling the 'let' scope
            let scopeVarStorage =
                env.VarStorage.Add(name, Storage.Reg(Reg.r(env.Target)))
            /// Environment for compiling the 'let' scope
            let scopeEnv = { env with Target = scopeTarget
                                      VarStorage = scopeVarStorage }
            initCode
                ++ (doCodegen scopeEnv scope)
                    .AddText(RV.MV(Reg.r(env.Target), Reg.r(scopeTarget)),
                             "Move 'let' scope result to 'let' target register")

    | LetRec(name, _, init, scope) ->
        /// 'let rec...' initialisation code, which leaves its result in the
        /// 'target' register (which we overwrite at the end of the 'scope'
        /// execution)
        let initCode = doCodegen env init
        match init.Type with
        | t when (isSubtypeOf init.Env t TUnit) ->
            // The 'init' produces a unit value, i.e. nothing: we can keep using
            // the same target registers, and we don't need to update the
            // variables-to-registers mapping.
            initCode ++ (doCodegen env scope)
        | t when (isSubtypeOf init.Env t TFloat) ->
            /// Target register for compiling the 'let rec' scope
            let scopeTarget = env.FPTarget + 1u
            /// Variable storage for compiling the 'let rec' scope
            let scopeVarStorage =
                env.VarStorage.Add(name, Storage.FPReg(FPReg.r(env.FPTarget)))
            /// Environment for compiling the 'let rec' scope
            let scopeEnv = { env with FPTarget = scopeTarget
                                      VarStorage = scopeVarStorage }
            initCode
                ++ (doCodegen scopeEnv scope)
                    .AddText(RV.FMV_S(FPReg.r(env.FPTarget),
                                      FPReg.r(scopeTarget)),
                             "Move result of 'let rec' scope expression into target register")
        | _ ->  // Default case for integer-like initialisation expressions
            /// Target register for compiling the 'let rec' scope
            let scopeTarget = env.Target + 1u
            /// Variable storage for compiling the 'let rec' scope
            let scopeVarStorage =
                env.VarStorage.Add(name, Storage.Reg(Reg.r(env.Target)))
            /// Environment for compiling the 'let rec' scope
            let scopeEnv = { env with Target = scopeTarget
                                      VarStorage = scopeVarStorage }
            initCode
                ++ (doCodegen scopeEnv scope)
                    .AddText(RV.MV(Reg.r(env.Target), Reg.r(scopeTarget)),
                             "Move 'let rec' scope result to 'let rec' target register")

    | Assign(lhs, rhs) ->
        match lhs.Expr with
        | Var(name) ->
            /// Code for the 'rhs', leaving its result in the target register
            let rhsCode = doCodegen env rhs
            match rhs.Type with
            | t when (isSubtypeOf rhs.Env t TUnit) ->
                rhsCode // No assignment to perform
            | _ ->
                match (env.VarStorage.TryFind name) with
                | Some(Storage.Reg(reg)) ->
                    rhsCode.AddText(RV.MV(reg, Reg.r(env.Target)),
                                    $"Assignment to variable %s{name}")
                | Some(Storage.FPReg(reg)) ->
                    rhsCode.AddText(RV.FMV_S(reg, FPReg.r(env.FPTarget)),
                                    $"Assignment to variable %s{name}")
                | Some(Storage.Label(lab)) ->
                    match rhs.Type with
                    | t when (isSubtypeOf rhs.Env t TFloat) ->
                        rhsCode.AddText([ (RV.LA(Reg.r(env.Target), lab),
                                           $"Load address of variable '%s{name}'")
                                          (RV.FSW_S(FPReg.r(env.FPTarget), Imm12(0),
                                                    Reg.r(env.Target)),
                                           $"Transfer value of '%s{name}' to memory") ])
                    | _ ->
                        rhsCode.AddText([ (RV.LA(Reg.r(env.Target + 1u), lab),
                                           $"Load address of variable '%s{name}'")
                                          (RV.SW(Reg.r(env.Target), Imm12(0),
                                                 Reg.r(env.Target + 1u)),
                                           $"Transfer value of '%s{name}' to memory") ])
                // Add this new case to handle Frame storage
                | Some(Storage.Frame(offset)) ->
                    match rhs.Type with
                    | t when (isSubtypeOf rhs.Env t TFloat) ->
                        rhsCode.AddText(RV.FSW_S(FPReg.r(env.FPTarget), Imm12(offset), Reg.fp),
                                    $"Assignment to stack variable %s{name} at fp+%d{offset}")
                    | _ ->
                        rhsCode.AddText(RV.SW(Reg.r(env.Target), Imm12(offset), Reg.fp),
                                    $"Assignment to stack variable %s{name} at fp+%d{offset}")
                | None -> failwith $"BUG: variable without storage: %s{name}"
        | FieldSelect(target, field) ->
            /// Assembly code for computing the 'target' object of which we are
            /// selecting the 'field'.  We write the computation result (which
            /// should be a struct memory address) in the target register.
            let selTargetCode = doCodegen env target
            /// Code for the 'rhs', leaving its result in the target+1 register
            let rhsCode = doCodegen {env with Target = env.Target + 1u} rhs
            match (expandType target.Env target.Type) with
            | TStruct(fields) ->
                /// Names of the struct fields
                let (fieldNames, _) = List.unzip fields
                /// Offset of the selected struct field from the beginning of
                /// the struct
                let offset = List.findIndex (fun f -> f = field) fieldNames
                /// Assembly code that performs the field value assignment
                let assignCode =
                    match rhs.Type with
                    | t when (isSubtypeOf rhs.Env t TUnit) ->
                        Asm() // Nothing to do
                    | t when (isSubtypeOf rhs.Env t TFloat) ->
                        Asm(RV.FSW_S(FPReg.r(env.FPTarget), Imm12(offset * 4),
                                     Reg.r(env.Target)),
                            $"Assigning value to struct field '%s{field}'")
                    | _ ->
                        Asm([(RV.SW(Reg.r(env.Target + 1u), Imm12(offset * 4),
                                    Reg.r(env.Target)),
                              $"Assigning value to struct field '%s{field}'")
                             (RV.MV(Reg.r(env.Target), Reg.r(env.Target + 1u)),
                              "Copying assigned value to target register")])
                // Put everything together
                selTargetCode ++ rhsCode ++ assignCode
            | t ->
                failwith $"BUG: field selection on invalid object type: %O{t}"
        | _ ->
            failwith ($"BUG: assignment to invalid target:%s{Util.nl}"
                      + $"%s{PrettyPrinter.prettyPrint lhs}")

    | While(cond, body) ->
        /// Label to mark the beginning of the 'while' loop
        let whileBeginLabel = Util.genSymbol "while_loop_begin"
        /// Label to mark the beginning of the 'while' loop body
        let whileBodyBeginLabel = Util.genSymbol "while_body_begin"
        /// Label to mark the end of the 'while' loop
        let whileEndLabel = Util.genSymbol "while_loop_end"
        // Check the 'while' condition, jump to 'whileEndLabel' if it is false.
        // Here we use a register to load the address of a label (using the
        // instruction LA) and then jump to it (using the instruction LR): this
        // way, the label address can be very far from the jump instruction
        // address --- and this can be important if the compilation of 'body'
        // produces a large amount of assembly code
        Asm(RV.LABEL(whileBeginLabel))
            ++ (doCodegen env cond)
                .AddText([
                    (RV.BNEZ(Reg.r(env.Target), whileBodyBeginLabel),
                     "Jump to loop body if 'while' condition is true")
                    (RV.LA(Reg.r(env.Target), whileEndLabel),
                     "Load address of label at the end of the 'while' loop")
                    (RV.JR(Reg.r(env.Target)), "Jump to the end of the loop")
                    (RV.LABEL(whileBodyBeginLabel),
                     "Body of the 'while' loop starts here")
                ])
            ++ (doCodegen env body)
            .AddText([
                (RV.LA(Reg.r(env.Target), whileBeginLabel),
                 "Load address of label at the beginning of the 'while' loop")
                (RV.JR(Reg.r(env.Target)), "Jump to the end of the loop")
                (RV.LABEL(whileEndLabel), "")
            ])

//dowhile
    | DoWhile(body, cond) ->
    /// Label for the body of the do-while loop
        let doWhileBodyLabel = Util.genSymbol "do_while_body"
    /// Label for the condition check
        let doWhileCondLabel = Util.genSymbol "do_while_cond"
    /// Label for the end of the do-while loop
        let doWhileEndLabel = Util.genSymbol "do_while_end"
    // First execute the body, without checking the condition
        Asm(RV.LABEL(doWhileBodyLabel), "Body of the 'do-while' loop starts here")
            ++ (doCodegen env body)
            .AddText(RV.LABEL(doWhileCondLabel), "Condition check for the 'do-while' loop")
        // Then check the condition
            ++ (doCodegen env cond)
            .AddText([
                (RV.BNEZ(Reg.r(env.Target), doWhileBodyLabel),
                 "Jump back to loop body if condition is true")
                (RV.LABEL(doWhileEndLabel), "End of the 'do-while' loop")
            ])

    | Lambda(args, body) ->
        /// Label to mark the position of the lambda term body
        let funLabel = Util.genSymbol "lambda"

        /// Names of the Lambda arguments
        let (argNames, _) = List.unzip args

        /// List of pairs associating each Lambda argument to its type.  We
        /// retrieve the type of each argument by looking into the environment
        /// used to type-check the Lambda 'body'
        let argNamesTypes = List.map (fun a -> (a, body.Env.Vars[a])) argNames

        /// Compiled function body
        let bodyCode = compileFunction argNamesTypes body env
        
        /// Compiled function code where the function label is located just
        /// before the 'bodyCode', and everything is placed at the end of the
        /// text segment (i.e. in the "PostText")
        let funCode =
            (Asm(RV.LABEL(funLabel), "Lambda term (i.e. function instance) code")
                ++ bodyCode).TextToPostText // Move to the end of text segment

        // Finally, load the function address (label) in the target register
        Asm(RV.LA(Reg.r(env.Target), funLabel), "Load lambda function address")
            ++ funCode

    | Application(expr, args) ->
        /// Integer registers to be saved on the stack before executing the
        /// function call, and restored when the function returns.  The list of
        /// saved registers excludes the target register for this application.
        /// Note: the definition of 'saveRegs' uses list comprehension:
        /// https://en.wikibooks.org/wiki/F_Sharp_Programming/Lists#Using_List_Comprehensions
        let saveRegs =
            List.except [Reg.r(env.Target)]
                        (Reg.ra :: [for i in 0u..7u do yield Reg.a(i)]
                         @ [for i in 0u..6u do yield Reg.t(i)])

        /// Assembly code for the expression being applied as a function
        let appTermCode =
            Asm().AddText(RV.COMMENT("Load expression to be applied as a function"))
            ++ (doCodegen env expr)

        /// Indexed list of argument expressions.  We will use the as an offset
        /// (above the current target register) to determine the target register
        /// for compiling each expression.
        let indexedArgs = List.indexed args
        /// Function that compiles an argument (using its index to determine its
        /// target register) and accumulates the generated assembly code
        let compileArg (acc: Asm) (i, arg) =
            acc ++ (doCodegen {env with Target = env.Target + (uint i) + 1u} arg)
        /// Assembly code of all application arguments, obtained by folding over
        /// 'indexedArgs'
        let argsCode = List.fold compileArg (Asm()) indexedArgs

        /// Function that copies the content of a target register (used by
        /// 'compileArgs' and 'argsCode' above) into an 'a' register, using an
        /// index to determine the source and target registers, and accumulating
        /// the generated assembly code
        let copyArg (acc: Asm) (i: int) =
            acc.AddText(RV.MV(Reg.a(uint i), Reg.r(env.Target + (uint i) + 1u)),
                        $"Load function call argument %d{i+1}")

        /// Code that loads each application argument into a register 'a', by
        /// copying the contents of the target registers used by 'compileArgs'
        /// and 'argsCode' above.  To this end, this code folds over the indexes
        /// of all arguments (from 0 to args.Length), using 'copyArg' above.
        /// Function that stores extra arguments (beyond the 8th) on the stack
        let storeExtraArg (acc: Asm) (i: int) =
            // Each argument takes 4 bytes (a word in RISC-V)
            // We start storing at sp+0 and work up
            acc.AddText(RV.SW(Reg.r(env.Target + (uint i) + 1u), Imm12((i - 8) * 4), Reg.sp),
                        $"Store extra function call argument %d{i+1} at sp+%d{(i - 8) * 4}")

        /// Code that loads the first 8 application arguments (if available) into registers 'a'
        let regArgsCount = min 8 args.Length
        let argsLoadCode = List.fold copyArg (Asm()) [0..(regArgsCount-1)]

        /// Code that stores extra arguments (beyond the 8th) on the stack
        let extraArgsCount = max 0 (args.Length - 8)
        let stackArgsStoreCode = 
            if extraArgsCount > 0 then
                // Adjust stack pointer to make room for extra arguments
                Asm(RV.ADDI(Reg.sp, Reg.sp, Imm12(-4 * extraArgsCount)),
                    $"Adjust stack pointer for %d{extraArgsCount} extra arguments")
                ++ (List.fold storeExtraArg (Asm()) [8..(args.Length-1)])
            else
                Asm() // No extra arguments to store

        /// Code that performs the function call
        let callCode =
            appTermCode
            ++ argsCode // Code to compute each argument of the function call
                .AddText(RV.COMMENT("Before function call: save caller-saved registers"))
                ++ (saveRegisters saveRegs [])
                ++ stackArgsStoreCode // Store extra arguments on stack if needed
                ++ argsLoadCode // Code to load arg values into arg registers
                    .AddText(RV.JALR(Reg.ra, Imm12(0), Reg.r(env.Target)), "Function call")

        /// Code to restore stack pointer if extra arguments were pushed
        let stackRestoreCode =
            if extraArgsCount > 0 then
                Asm(RV.ADDI(Reg.sp, Reg.sp, Imm12(4 * extraArgsCount)),
                    "Restore stack pointer after function call")
            else
                Asm() // No stack adjustment needed


        /// Code that handles the function return value (if any)
        let retCode =
            Asm(RV.MV(Reg.r(env.Target), Reg.a0),
                $"Copy function return value to target register")

        // Put everything together, and restore the caller-saved registers
        callCode
            .AddText(RV.COMMENT("After function call"))
            ++ stackRestoreCode // Restore stack pointer if needed
            ++ retCode
            .AddText(RV.COMMENT("Restore caller-saved registers"))
                ++ (restoreRegisters saveRegs [])

    | StructCons(fields) ->
        // To compile a structure constructor, we allocate heap space for the
        // whole struct instance, and then compile its field initialisations
        // one-by-one, storing each result in the corresponding heap location.
        // The struct heap address will end up in the 'target' register - i.e.
        // the register will contain a pointer to the first element of the
        // allocated structure
        let (fieldNames, fieldInitNodes) = List.unzip fields
        /// Generate the code that initialises a struct field, and accumulates
        /// the result.  This function is folded over all indexed struct fields,
        /// to produce the assembly code that initialises all fields.
        let folder = fun (acc: Asm) (fieldOffset: int, fieldInit: TypedAST) ->
            /// Code that initialises a single struct field.  Each field init
            /// result is compiled by targeting the register (target+1u),
            /// because the 'target' register holds the base memory address of
            /// the struct.  After the init result for a field is computed, we
            /// copy it into its heap location, by adding the field offset
            /// (multiplied by 4, i.e. the word size) to the base struct address
            let fieldInitCode: Asm =
                match fieldInit.Type with
                | t when (isSubtypeOf fieldInit.Env t TUnit) ->
                    Asm() // Nothing to do
                | t when (isSubtypeOf fieldInit.Env t TFloat) ->
                    Asm(RV.FSW_S(FPReg.r(env.FPTarget), Imm12(fieldOffset * 4),
                                 Reg.r(env.Target)),
                        $"Initialize struct field '%s{fieldNames.[fieldOffset]}'")
                | _ ->
                    Asm(RV.SW(Reg.r(env.Target + 1u), Imm12(fieldOffset * 4),
                              Reg.r(env.Target)),
                        $"Initialize struct field '%s{fieldNames.[fieldOffset]}'")
            acc ++ (doCodegen {env with Target = env.Target + 1u} fieldInit)
                ++ fieldInitCode
        /// Assembly code for initialising each field of the struct, by folding
        /// the 'folder' function above over all indexed struct fields (we use
        /// the index to know the offset of a field from the beginning of the
        /// struct)
        let fieldsInitCode =
            List.fold folder (Asm()) (List.indexed fieldInitNodes)

        /// Assembly code that allocates space on the heap for the new
        /// structure, through an 'Sbrk' system call.  The size of the structure
        /// is computed by multiplying the number of fields by the word size (4)
        let structAllocCode =
            (beforeSysCall [Reg.a0] [])
                .AddText([
                    (RV.LI(Reg.a0, fields.Length * 4),
                     "Amount of memory to allocate for a struct (in bytes)")
                    (RV.LI(Reg.a7, 9), "RARS syscall: Sbrk")
                    (RV.ECALL, "")
                    (RV.MV(Reg.r(env.Target), Reg.a0),
                     "Move syscall result (struct mem address) to target")
                ])
                ++ (afterSysCall [Reg.a0] [])

        // Put everything together: allocate heap space, init all struct fields
        structAllocCode ++ fieldsInitCode

    | FieldSelect(target, field) ->
        // To compile a field selection, we first execute the 'target' object of
        // the field selection, whose code is expected to leave a struct memory
        // address in the environment's 'target' register; then use the 'target'
        // type to determine the memory offset where the selected field is
        // located, and retrieve its value.

        /// Generated code for the target object whose field is being selected
        let selTargetCode = doCodegen env target
        /// Assembly code to access the struct field in memory (depending on the
        /// 'target' type) and leave its value in the target register
        let fieldAccessCode =
            match (expandType node.Env target.Type) with
            | TStruct(fields) ->
                let (fieldNames, fieldTypes) = List.unzip fields
                let offset = List.findIndex (fun f -> f = field) fieldNames
                match fieldTypes.[offset] with
                | t when (isSubtypeOf node.Env t TUnit) ->
                    Asm() // Nothing to do
                | t when (isSubtypeOf node.Env t TFloat) ->
                    Asm(RV.FLW_S(FPReg.r(env.FPTarget), Imm12(offset * 4),
                                 Reg.r(env.Target)),
                        $"Retrieve value of struct field '%s{field}'")
                | _ ->
                    Asm(RV.LW(Reg.r(env.Target), Imm12(offset * 4),
                              Reg.r(env.Target)),
                        $"Retrieve value of struct field '%s{field}'")
            | t ->
                failwith $"BUG: FieldSelect codegen on invalid target type: %O{t}"

        // Put everything together: compile the target, access the field
        selTargetCode ++ fieldAccessCode
    | Match(expr, cases) ->
        failwith "Match expressions are not supported in the current version of the compiler"
    | Pointer(_) ->
        failwith "BUG: pointers cannot be compiled (by design!)"
    
    | UnionCons(label, expr) ->
        failwith "UnionCons expressions not yet fully supported in codegen"

/// Generate code to save the given registers on the stack, before a RARS system
/// call. Register a7 (which holds the system call number) is backed-up by
/// default, so it does not need to be specified when calling this function.
and internal beforeSysCall (regs: List<Reg>) (fpregs: List<FPReg>): Asm =
    Asm(RV.COMMENT("Before system call: save registers"))
        ++ (saveRegisters (Reg.a7 :: regs) fpregs)

/// Generate code to restore the given registers from the stack, after a RARS
/// system call. Register a7 (which holds the system call number) is restored
/// by default, so it does not need to be specified when calling this function.
and internal afterSysCall (regs: List<Reg>) (fpregs: List<FPReg>): Asm =
    Asm(RV.COMMENT("After system call: restore registers"))
        ++ (restoreRegisters (Reg.a7 :: regs) fpregs)

/// Generate code to save the given lists of registers by using increasing
/// offsets from the stack pointer register (sp).
and internal saveRegisters (rs: List<Reg>) (fprs: List<FPReg>): Asm =
    /// Generate code to save standard registers by folding over indexed 'rs'
    let regSave (asm: Asm) (i, r) = asm.AddText(RV.SW(r, Imm12(i * 4), Reg.sp))
    /// Code to save standard registers
    let rsSaveAsm = List.fold regSave (Asm()) (List.indexed rs)

    /// Generate code to save floating point registers by folding over indexed
    /// 'fprs', and accumulating code on top of 'rsSaveAsm' above. Notice that
    /// we use the length of 'rs' as offset for saving on the stack, since those
    /// stack locations are already used to save 'rs' above.
    let fpRegSave (asm: Asm) (i, r) =
        asm.AddText(RV.FSW_S(r, Imm12((i + rs.Length) * 4), Reg.sp))
    /// Code to save both standard and floating point registers
    let regSaveCode = List.fold fpRegSave rsSaveAsm (List.indexed fprs)

    // Put everything together: update the stack pointer and save the registers
    Asm(RV.ADDI(Reg.sp, Reg.sp, Imm12(-4 * (rs.Length + fprs.Length))),
        "Update stack pointer to make room for saved registers")
      ++ regSaveCode

/// Generate code to restore the given lists of registers, that are assumed to
/// be saved with increasing offsets from the stack pointer register (sp)
and internal restoreRegisters (rs: List<Reg>) (fprs: List<FPReg>): Asm =
    /// Generate code to restore standard registers by folding over indexed 'rs'
    let regLoad (asm: Asm) (i, r) = asm.AddText(RV.LW(r, Imm12(i * 4), Reg.sp))
    /// Code to restore standard registers
    let rsLoadAsm = List.fold regLoad (Asm()) (List.indexed rs)

    /// Generate code to restore floating point registers by folding over
    /// indexed 'fprs', and accumulating code on top of 'rsLoadAsm' above.
    /// Notice that we use the length of 'rs' as offset for saving on the stack,
    /// since those stack locations are already used to save 'rs' above.
    let fpRegLoad (asm: Asm) (i, r) =
        asm.AddText(RV.FLW_S(r, Imm12((i + rs.Length) * 4), Reg.sp))
    /// Code to restore both standard and floating point registers
    let regRestoreCode = List.fold fpRegLoad rsLoadAsm (List.indexed fprs)

    // Put everything together: restore the registers and then the stack pointer
    regRestoreCode
        .AddText(RV.ADDI(Reg.sp, Reg.sp, Imm12(4 * (rs.Length + fprs.Length))),
                 "Restore stack pointer after register restoration")

/// Compile a function instance with the given (optional) name, arguments, and
/// body, and using the given environment.  This function places all the
/// assembly code it generates in the Text segment (hence, this code may need
/// to be moved afterwards).
and internal compileFunction (args: List<string * Type>)
                             (body: TypedAST)
                             (env: CodegenEnv): Asm =
    /// List of indexed arguments: we use the index as the number of the 'a'
    /// register that holds the argument
    let indexedArgs = List.indexed args
    /// Folder function that assigns storage information to function arguments:
    /// it assigns an 'a' register to each function argument, and accumulates
    /// the result in a mapping (that will be used as env.VarStorage)
    let folder (acc: Map<string, Storage>) (i, (var, _tpe)) =
        if i < 8 then
            // First 8 args use registers a0-a7
            acc.Add(var, Storage.Reg(Reg.a((uint)i)))
        else
            // Args beyond 8 use stack locations relative to fp
            // Each parameter is 4 bytes (a word in RISC-V)
            acc.Add(var, Storage.Frame((i-8)*4))
    /// Updated storage information including function arguments
    let varStorage2 = List.fold folder env.VarStorage indexedArgs

    /// Code for the body of the function, using the newly-created
    /// variable storage mapping 'varStorage2'.  NOTE: the function body
    /// compilation restarts the target register numbers from 0.  Consequently,
    /// the function body result (i.e. the function return value) will be stored
    /// in Reg.r(0) or FPReg.r(0) (depending on its type); when the function
    /// ends, we need to move that result into the function return value
    /// register 'a0' or 'fa0'.
    let bodyCode =
        let env = {Target = 0u; FPTarget = 0u; VarStorage = varStorage2}
        doCodegen env body
    /// Code to move the body result into the function return value register
    let returnCode =
        Asm(RV.MV(Reg.a0, Reg.r(0u)),
            "Move result of function into return value register")

    /// Integer registers to save before executing the function body.
    /// Note: the definition of 'saveRegs' uses list comprehension:
    /// https://en.wikibooks.org/wiki/F_Sharp_Programming/Lists#Using_List_Comprehensions
    let saveRegs = [for i in 0u..11u do yield Reg.s(i)]

    // Finally, we put together the full code for the function
    Asm(RV.COMMENT("Funtion prologue begins here"))
            .AddText(RV.COMMENT("Save callee-saved registers"))
        ++ (saveRegisters saveRegs [])
            .AddText(RV.ADDI(Reg.fp, Reg.sp, Imm12(saveRegs.Length * 4)),
                     "Update frame pointer for the current function")
            .AddText(RV.COMMENT("End of function prologue.  Function body begins"))
        ++ bodyCode
            .AddText(RV.COMMENT("End of function body.  Function epilogue begins"))
        ++ returnCode
            .AddText(RV.COMMENT("Restore callee-saved registers"))
            ++ (restoreRegisters saveRegs [])
                .AddText(RV.JR(Reg.ra), "End of function, return to caller")


/// Generate RISC-V assembly for the given AST.
let codegen (node: TypedAST): RISCV.Asm =
    /// Initial codegen environment, targeting generic registers 0 and without
    /// any variable in the storage map
    let env = {Target = 0u; FPTarget = 0u; VarStorage =  Map[]}
    Asm(RV.MV(Reg.fp, Reg.sp), "Initialize frame pointer")
    ++ (doCodegen env node)
        .AddText([
            (RV.LI(Reg.a7, 10), "RARS syscall: Exit")
            (RV.ECALL, "Successful exit with code 0")
        ])

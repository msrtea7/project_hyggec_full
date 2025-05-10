/// Module for computing the least upper bound (LUB) of types
module TypeInference

open Type

/// Determines if type t1 is a subtype of type t2
let rec isSubtypeOf (t1: Type) (t2: Type): bool =
    match t1, t2 with
    | TBool, TBool
    | TInt, TInt
    | TFloat, TFloat
    | TString, TString
    | TUnit, TUnit -> true

    | TVar name1, TVar name2 -> name1 = name2

    // Handle the case where one is a union type and one is a type variable
    // This is needed for pattern matching expressions where the union cases 
    // should be compatible with the declared type variable
    | TUnion(_), TVar(_) -> true  // Union type is considered a subtype of a type variable
                                  // This assumes type checking environment validates properly

    // Handle the case where unions with subset of cases are subtypes
    // For example, union {Some: int} is a subtype of union {Some: int; None: unit}
    | TUnion(cases1), TUnion(cases2) when cases1.Length < cases2.Length ->
        // Check if all cases in the smaller union are compatible with the larger union
        let caseMap1 = Map.ofList cases1
        let caseMap2 = Map.ofList cases2
        
        Map.forall (fun caseName caseType1 ->
            match Map.tryFind caseName caseMap2 with
            | Some caseType2 -> isSubtypeOf caseType1 caseType2
            | None -> false
        ) caseMap1

    | TFun(args1, ret1), TFun(args2, ret2) ->
        if args1.Length <> args2.Length then false
        else
            let argSubtypes = List.zip args2 args1 |> List.forall (fun (a2, a1) -> isSubtypeOf a2 a1)
            let retSubtype = isSubtypeOf ret1 ret2
            argSubtypes && retSubtype

    | TStruct(fields1), TStruct(fields2) ->
        let fieldMap1 = Map.ofList fields1
        let fieldMap2 = Map.ofList fields2
        Map.forall (fun fieldName fieldType2 ->
            match Map.tryFind fieldName fieldMap1 with
            | Some fieldType1 -> isSubtypeOf fieldType1 fieldType2
            | None -> false
        ) fieldMap2

    | TUnion(cases1), TUnion(cases2) ->
        let caseMap1 = Map.ofList cases1
        let caseMap2 = Map.ofList cases2
        Map.forall (fun caseName caseType1 ->
            match Map.tryFind caseName caseMap2 with
            | Some caseType2 -> isSubtypeOf caseType1 caseType2
            | None -> false
        ) caseMap1

    | _ -> false


/// Computes the least upper bound (LUB) of two types
/// 
/// The LUB of two types T and T' is defined as:
/// 1. If T is a subtype of T', then the LUB is T'
/// 2. If T' is a subtype of T, then the LUB is T
/// 3. If T and T' are both union types, the LUB is a union containing all labels from both,
///    with the LUB of corresponding case types
/// 4. If T and T' are both struct types, the LUB is a struct containing only common fields,
///    with the LUB of corresponding field types
/// 5. For function types, the LUB requires matching arities and compatible argument/return types
/// 6. If none of the above apply, the types are incompatible and have no LUB
let rec computeLUB (t1: Type) (t2: Type): Type =
    if isSubtypeOf t1 t2 then t2
    elif isSubtypeOf t2 t1 then t1
    else
        match t1, t2 with
        | TUnion(cases1), TUnion(cases2) ->
            let allLabels =
                List.map fst cases1 @ List.map fst cases2
                |> List.distinct
            let mergedCases =
                allLabels
                |> List.map (fun label ->
                    let type1Opt = List.tryFind (fun (l, _) -> l = label) cases1 |> Option.map snd
                    let type2Opt = List.tryFind (fun (l, _) -> l = label) cases2 |> Option.map snd
                    match type1Opt, type2Opt with
                    | Some t1, Some t2 -> (label, computeLUB t1 t2)
                    | Some t1, None -> (label, t1)
                    | None, Some t2 -> (label, t2)
                    | None, None -> failwith $"Impossible: label '{label}' not found in either union"
                )
            TUnion(mergedCases)

        // Special case for union type and type variable
        | TUnion(_), TVar(_) -> t2  // TVar is the supertype
        | TVar(_), TUnion(_) -> t1  // TVar is the supertype

        | TStruct(fields1), TStruct(fields2) ->
            // Definition of LUB for struct types:
            // - T''(LUB) only contains fields common to both T and T'
            // - For each common field f, the type in T'' is the LUB of T.f and T'.f
            // - LUB exists if and only if:
            //   1. There is at least one common field
            //   2. For each common field, the LUB of its types in T and T' exists
            
            // Get field names from both structs
            let fieldNames1 = List.map fst fields1 |> Set.ofList
            let fieldNames2 = List.map fst fields2 |> Set.ofList
            // Find common field names
            let commonFieldNames = Set.intersect fieldNames1 fieldNames2
            
            // Condition 1: LUB exists only if there's at least one common field
            if Set.isEmpty commonFieldNames then
                failwith $"LUB does not exist: struct types {t1} and {t2} have no common fields"
            else
                // Attempt to compute LUB for each common field
                let fieldLUBResults = 
                    commonFieldNames
                    |> Set.toList
                    |> List.map (fun fieldName ->
                        let type1 = List.find (fun (name, _) -> name = fieldName) fields1 |> snd
                        let type2 = List.find (fun (name, _) -> name = fieldName) fields2 |> snd
                        try
                            // Try to compute LUB for this field
                            (fieldName, Some (computeLUB type1 type2))
                        with ex ->
                            // If LUB computation fails for this field, capture the error
                            (fieldName, None)
                    )
                
                // Extract field names with failed LUB computation
                let failedFields = fieldLUBResults |> List.filter (fun (_, result) -> result.IsNone) |> List.map fst
                
                // Condition 2: LUB exists only if all common fields have a valid LUB
                if not (List.isEmpty failedFields) then
                    let fieldsStr = String.concat ", " failedFields
                    failwith $"LUB does not exist: fields '{fieldsStr}' between struct types {t1} and {t2} have incompatible types"
                else
                    // Both conditions met, construct the LUB struct with successful field LUBs
                    let computedFields = fieldLUBResults |> List.map (fun (name, result) -> (name, result.Value))
                    TStruct(computedFields)

        | TFun(args1, ret1), TFun(args2, ret2) when args1.Length = args2.Length ->
            // For function types, arguments are contravariant (GLB) and return type is covariant (LUB)
            let lubArgs =
                List.map2 (fun a1 a2 ->
                    if isSubtypeOf a2 a1 then a2
                    elif isSubtypeOf a1 a2 then a1
                    else failwith $"Cannot compute GLB for function argument types: {a1} vs {a2}"
                ) args1 args2
            let lubRet = computeLUB ret1 ret2
            TFun(lubArgs, lubRet)

        | TFun(args1, _), TFun(args2, _) ->
            failwith $"Cannot compute LUB for functions with different arities: {args1.Length} vs {args2.Length}"

        | _ ->
            failwith $"Cannot compute LUB for incompatible types: {t1} and {t2}"


/// Computes the least upper bound of a list of types
let computeLUBMany (types: Type list): Type =
    match types with
    | [] -> failwith "Cannot compute LUB of empty list of types"
    | [t] -> t
    | t :: ts -> List.fold computeLUB t ts


/// Modified type checking for pattern matching that uses LUB
/// This function computes the LUB of all case types to determine
/// the result type of a pattern matching expression
let typeCheckPatternMatch (cases: Type list): Type =
    computeLUBMany cases
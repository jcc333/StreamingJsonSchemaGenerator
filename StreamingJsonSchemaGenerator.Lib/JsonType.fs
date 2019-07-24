namespace StreamingJsonSchemaGenerator

module JsonType =
    open System
    open Token

    type TypeTree =
    | Union of Set<TypeTree>
    | Object of Map<string, TypeTree>
    | Array of TypeTree
    | Number
    | String
    | Null
    | Boolean
    | Undefined
    | Date
    | Bytes

    let rec unify(this: TypeTree)(that: TypeTree) =
        match this, that with
        // Exact matches
        | _ when this = that -> this

        // Any type is better than the undefined type
        | Undefined, _ -> that
        | _, Undefined -> this

        // For two objects, each of their keys-in-common unified plus each of their distinct types
        | Object(leftProperties), Object(rightProperties) -> 
            // Merge two maps with a given function to deal with shared keys
            let merge (a : Map<'a, 'b>) (b : Map<'a, 'b>) (f : 'b -> 'b -> 'b): Map<'a, 'b> =
                let mergeStep table key value =
                    match Map.tryFind key table with
                    | Some v' -> Map.add key (f value v') table
                    | None -> Map.add key value table
                Map.fold mergeStep a b
            Object(merge leftProperties rightProperties unify)

        // For two arrays, their unified type is an array of the union of their elementary types
        | Array(leftElements), Array(rightElements) -> Array(unify leftElements rightElements)

        // For a union and an array, inject the array element type into whatever array appears in the union if any
        | Union(lhs), Union(rhs) -> 
            Seq.fold unify Undefined (lhs + rhs)
        | Union(types), Array(elt) -> Union(types.Add(that))
        | Array(elt), Union(types) -> Union(types.Add(this))

        // For a union and an object, unify any object type in the union
        | Union(leftTypes), Object(_)  ->
            let objects, notObjects = Set.partition (function Object(_) -> true | _ -> false) leftTypes
            objects
            |> Set.fold unify that
            |> Set.singleton
            |> Set.union notObjects
            |> Union
        | Object(_), Union(_) -> unify that this

        | Union(leftTypes), scalar  -> Union(leftTypes.Add(that))
        | scalar, Union(rightTypes) -> Union(rightTypes.Add(this))

        // For two scalar types, create the union of the two types
        | this, that -> Union(Set.empty.Add(this).Add(that))

    type TypeTreeResult = Result<TypeTree, string>

    let unexpected(t: Token) =
        Error(String.Format("Unexpected token '{}'", t))

    let earlyEof =
        Error(String.Format("Unexpected EOF"))

    let typeOf(tokens: TokenSeq): Result<TypeTree, string> =
        let rec loop(tokens: TokenSeq): Result<TypeTree, string> =
            let handleToken = function
                | StartObject     -> objectType(Undefined, Map.empty, tokens)
                | StartArray      -> arrayType(Undefined, tokens)
                | Comment         -> loop(Seq.tail tokens)
                | Token.Number    -> Ok(Number)
                | Token.String    -> Ok(String)
                | Token.Boolean   -> Ok(Boolean)
                | Token.Null      -> Ok(Null)
                | Token.Undefined -> Ok(Undefined)
                | Token.Date      -> Ok(Date)
                | Token.Bytes     -> Ok(Bytes)
                | t               -> unexpected(t)
            consume (fun _ -> true) handleToken earlyEof tokens

        and objectType(acc: TypeTree, properties: Map<string, TypeTree>, tokens: TokenSeq) =
            let isEndOfObject = function
                | Ok(EndObject) -> false
                | _ -> true
            let handleToken = function
                | Comment -> objectType(acc, properties, Seq.tail tokens)
                | PropertyName(s) ->
                    match loop(Seq.tail tokens) with
                    | Ok(newType) ->
                        match properties.TryFind(s) with
                        | Some(oldType) -> Ok(unify oldType newType)
                        | None -> earlyEof
                    | Error(err) -> Error(err)
                | EndObject -> Ok(acc)
                | t -> unexpected(t)
            consume isEndOfObject handleToken earlyEof tokens

        and arrayType(acc, tokens) =
            let isEndOfArray = function
                | Ok(EndArray) -> false
                | _ -> true
            let handleToken =
                function
                | EndArray -> Ok(acc)
                | Comment -> arrayType(acc, Seq.tail tokens)
                | _ -> loop(tokens) |> Result.map (unify acc)
            consume isEndOfArray handleToken earlyEof tokens

        loop(tokens)

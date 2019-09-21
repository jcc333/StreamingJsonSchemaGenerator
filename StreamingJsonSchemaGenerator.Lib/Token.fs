namespace StreamingJsonSchemaGenerator

open System
open Newtonsoft.Json

module Token =
    type Token =
    | StartObject
    | StartArray
    | PropertyName of string
    | Comment
    | Number
    | String
    | Boolean
    | Null
    | Undefined
    | EndObject
    | EndArray
    | Date
    | Bytes

    type TokenResult = Result<Token, string>

    type TokenSeq = seq<TokenResult>

    let wrapTokensOut(typ: JsonToken, tok: obj): TokenResult =
        match typ with
        | JsonToken.None -> Error("Reader has not yet read.")
        | JsonToken.StartObject -> Result.Ok(StartObject)
        | JsonToken.StartArray -> Ok(StartArray)
        | JsonToken.PropertyName -> Ok(PropertyName(string tok))
        | JsonToken.Comment -> Ok(Comment) | JsonToken.Integer -> Ok(Number)
        | JsonToken.Float -> Ok(Number)
        | JsonToken.String -> Ok(String)
        | JsonToken.Boolean -> Ok(Boolean)
        | JsonToken.Null -> Ok(Null)
        | JsonToken.Undefined -> Ok(Undefined)
        | JsonToken.EndObject -> Ok(EndObject)
        | JsonToken.EndArray -> Ok(EndArray)
        | JsonToken.Date -> Ok(Date)
        | JsonToken.Bytes -> Ok(Bytes)
        | _ -> Error(String.Format("Json '{}': '{}' out of range or out of scope", typ, tok))

    type StakeStackItem = ArrayScope | ObjectScope

    let validateTokenOrder(ts: TokenResult seq) =
        let rec loop(stack: StakeStackItem list)(ts: TokenResult seq): TokenResult seq =
            seq {
                let h = Seq.tryHead ts
                let hs = Seq.tail ts
                match h with
                | None ->
                    yield! Seq.empty
                | Some(Ok(StartObject)) ->
                    yield h.Value
                    let arrStack = ObjectScope::stack
                    yield! loop arrStack hs
                | Some(Ok(StartArray)) ->
                    yield h.Value
                    let objStack = ArrayScope::stack
                    yield! loop objStack hs
                | Some(Ok(PropertyName(_))) when List.tryHead stack = Some(ObjectScope) ->
                    yield h.Value
                    yield! loop stack hs
                | Some(Ok(PropertyName(_))) ->
                    yield Error("property names belong in objects")
                | Some(Ok(EndObject)) when List.tryHead stack = Some(ObjectScope) -> 
                    yield h.Value
                    let nonObjStack = List.tail stack
                    yield! loop nonObjStack hs
                | Some(Ok(EndObject)) -> 
                    yield Error("ends of objects belong in objects")
                | Some(Ok(EndArray)) when List.tryHead stack = Some(ArrayScope) -> 
                    yield h.Value
                    let nonArrStack = List.tail stack
                    yield! loop nonArrStack hs
                | Some(Ok(EndArray)) ->
                    yield Error("ends of arrays belong in arrays")
                | _ ->
                    yield h.Value
                    yield! loop stack hs
            }
        loop [] ts

    let tokens(reader: Newtonsoft.Json.JsonReader) =
        seq { while reader.Read() do yield (reader.TokenType, reader.Value) }

    let tokenize(reader: Newtonsoft.Json.JsonReader) =
        reader |> tokens |> Seq.map wrapTokensOut |> validateTokenOrder 

    let consume(tokens: TokenSeq)(until: TokenResult -> Boolean)(tokenizer: Token -> Result<'a, string>)(exhausted: Result<'a, string>): Result<'a, string> =
        tokens
        |> Seq.takeWhile until
        |> Seq.take 1
        |> Seq.tryHead
        |> Option.map (Result.bind tokenizer)
        |> Option.defaultValue exhausted
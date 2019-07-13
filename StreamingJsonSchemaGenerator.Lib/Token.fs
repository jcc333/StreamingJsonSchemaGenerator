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

    let validate(typ: JsonToken, tok: obj): TokenResult =
        match typ with
        | JsonToken.None -> Error("Reader has not yet read.")
        | JsonToken.StartObject -> Result.Ok(StartObject)
        | JsonToken.StartArray -> Ok(StartArray)
        | JsonToken.PropertyName -> Ok(PropertyName(string tok))
        | JsonToken.Comment -> Ok(Comment)
        | JsonToken.Integer -> Ok(Number)
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

    let tokens(reader: Newtonsoft.Json.JsonReader) =
        seq { while reader.Read() do yield (reader.TokenType, reader.Value) }

    let tokenize(reader: Newtonsoft.Json.JsonReader) =
        reader |> tokens |> Seq.map validate

    let consume(tokens: TokenSeq)(until: TokenResult -> Boolean)(tokenizer: Token -> Result<'a, string>)(exhausted: Result<'a, string>): Result<'a, string> =
        tokens
        |> Seq.takeWhile until
        |> Seq.take 1
        |> Seq.tryHead
        |> Option.map (Result.bind tokenizer)
        |> Option.defaultValue exhausted
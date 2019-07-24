namespace StreamingJsonSchemaGenerator

namespace StreamingJsonSchema
    open System
    open Newtonsoft.Json.Schema
    open StreamingJsonSchemaGenerator.Token
    open StreamingJsonSchemaGenerator.JsonType

    type Generator(reader: Newtonsoft.Json.JsonReader) = class
        let typeTreeToSchema: TypeTree -> JSchema =
            let rec loop (s: JSchema) (t: TypeTree) =
                match t with
                | Union(members) ->
                    for t in members do
                        s.AnyOf.Add(loop s t)
                    s
                | Object(properties) ->
                    for p in properties do
                        s.Properties.Add(p.Key, loop s p.Value)
                    s
                | Array(elements) ->
                    let arraySchema = JSchema(Type = Nullable(JSchemaType.Array))
                    arraySchema.Items.Add(loop s elements)
                    arraySchema
                | Number -> JSchema(Type = Nullable(JSchemaType.Number))
                | String -> JSchema(Type = Nullable(JSchemaType.String))
                | Null -> JSchema(Type = Nullable(JSchemaType.Null))
                | Boolean -> JSchema(Type = Nullable(JSchemaType.Boolean))
                | Date -> JSchema(Type = Nullable(JSchemaType.String)) // this is probably wrong
                | Undefined -> JSchema(Type = Nullable(JSchemaType.None))
                | Bytes -> JSchema(Type = Nullable(JSchemaType.String)) // this is also probably wrong
            loop (JSchema())

        member this.Schema() =
            reader
            |> tokenize
            |> typeOf
            |> Result.map typeTreeToSchema
            |> function Ok(s) -> s | Error(err) -> failwith err
    end

namespace Tests

open NUnit.Framework
open StreamingJsonSchemaGenerator
open System
open System.IO
open Newtonsoft.Json
open JsonType

[<TestFixture>]
type JsonTypeTest () =
    let assertType jsonText expectedType =
        use textReader = new StringReader(jsonText)
        use jsonReader = new Newtonsoft.Json.JsonTextReader(textReader)
        let jsonTokenSeq = Token.tokenize(jsonReader)
        let jsonTypeactualType = JsonType.typeOf(jsonTokenSeq)
        Assert.AreEqual(jsonTypeactualType, Result<JsonType.TypeTree, string>.Ok(expectedType))

    [<Test>]
    member this.TestTypesAnInt() =
        let jsonText = @"1"
        let expectedType = JsonType.TypeTree.Number
        assertType jsonText expectedType

    [<Test>]
    member this.TestTypesAString() =
        let jsonText = "\"string\""
        let expectedType = JsonType.TypeTree.String
        assertType jsonText expectedType

    [<Test>]
    member this.TestTypesNull() =
        let jsonText = "null"
        let expectedType = JsonType.TypeTree.Null
        assertType jsonText expectedType

    [<Test>]
    member this.TypesEmptyArray() =
        let jsonText = "[]"
        let expectedType = JsonType.TypeTree.Array(JsonType.TypeTree.Undefined)
        assertType jsonText expectedType

    [<Test>]
    member this.TypesMonomorphicArray() =
        let jsonText = "[1,2,3,4,5.0]"
        let expectedType = JsonType.TypeTree.Array(JsonType.TypeTree.Number)
        assertType jsonText expectedType


    [<Test>]
    member this.TypesPolymorphicArray() =
        let jsonText = "[1,\"2\",3,4,5.0]"
        let expectedType = JsonType.TypeTree.Array(JsonType.TypeTree.Union(Set.ofList [JsonType.TypeTree.Number; JsonType.TypeTree.String]))
        assertType jsonText expectedType


    [<Test>]
    member this.TypesEmptyObject() =
        let jsonText = "{}"
        let expectedType = JsonType.TypeTree.Object(Map.empty<string, JsonType.TypeTree>)
        assertType jsonText expectedType

namespace Tests

open NUnit.Framework
open StreamingJsonSchemaGenerator
open System
open System.IO
open Newtonsoft.Json
open Token

[<TestFixture>]
type TokenTest () =
    let assertToken l r =
        use textReader = new StringReader(l)
        use jsonReader = new Newtonsoft.Json.JsonTextReader(textReader)
        let jsonTokenSeq = Token.tokenize(jsonReader)
        let token = Seq.head jsonTokenSeq
        Assert.AreEqual(token, Result<Token.Token, string>.Ok(r))

    let assertTokens l r =
        use textReader = new StringReader(l)
        use jsonReader = new Newtonsoft.Json.JsonTextReader(textReader)
        let jsonTokenSeq = Token.tokenize(jsonReader)
        let jsonTokenList = jsonTokenSeq |> List.ofSeq
        let expectedTokens = List.map Result<Token.Token, string>.Ok r
        Assert.AreEqual(jsonTokenList, expectedTokens)


    [<SetUp>]
    member this.Setup() = ()

    [<Test>]
    member this.TokenizesAnArray() =
        let jsonText = @"[1,2,""string"",4]"
        let expectedTokens =  [
            Token.StartArray;
            Token.Number;
            Token.Number;
            Token.String;
            Token.Number;
            Token.EndArray
        ]
        assertTokens jsonText expectedTokens

    [<Test>]
    member this.TokenizesANaturalNumber() =
        let jsonText = @"1"
        let expectedToken = Token.Number
        assertToken jsonText expectedToken

    [<Test>]
    member this.TokenizesAnInteger() =
        let jsonText = @"-1"
        let expectedToken = Token.Number
        assertToken jsonText expectedToken

    [<Test>]
    member this.TokenizesAFloat() =
        let jsonText = @"1.1"
        let expectedToken = Token.Number
        assertToken jsonText expectedToken

    [<Test>]
    member this.TokenizesAString() =
        let jsonText = @"""1"""
        let expectedToken = Token.String
        assertToken jsonText expectedToken

    [<Test>]
    member this.TokenizesABoolean() =
        let jsonText = "true"
        let expectedToken = Token.Boolean
        assertToken jsonText expectedToken

    [<Test>]
    member this.TokenizesAComment() =
        let jsonText = "// TODO: implement features"
        let expectedToken = Token.Comment
        assertToken jsonText expectedToken

    [<Test>]
    member this.TokenizesNull() =
        let jsonText = "null"
        let expectedToken = Token.Null
        assertToken jsonText expectedToken

    [<Test>]
    member this.TokenizesAnObject() =
        let jsonText = @"{""foo"":{""bar"":""baz""},""bork"":[1,2,3,[]]}"
        let expectedTokens = [
            Token.StartObject;
            Token.PropertyName("foo");
            Token.StartObject;
            Token.PropertyName("bar");
            Token.String;
            Token.EndObject;
            Token.PropertyName("bork");
            Token.StartArray;
            Token.Number;
            Token.Number;
            Token.Number;
            Token.StartArray;
            Token.EndArray
            Token.EndArray
            Token.EndObject;
        ]
        assertTokens jsonText expectedTokens

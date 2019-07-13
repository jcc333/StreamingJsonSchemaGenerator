namespace Tests

open NUnit.Framework
open StreamingJsonSchemaGenerator

[<TestClass>]
type TokenTests () =

    [<SetUp>]
    member this.Setup() =
        ()

    [<Test>]
    member this.AlwaysPasses() =
        Assert.Pass()
    
    [<Test>]
    member this.AlwaysFails() =
        Assert.Fail()

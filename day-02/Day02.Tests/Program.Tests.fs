module Day02.Tests

open NUnit.Framework
open Program

[<Test>]
let ``move should return correct end position `` () =
    let commands =
        [| "forward 5"
           "down 5"
           "forward 8"
           "up 3"
           "down 8"
           "forward 2" |]

    let actual = move commands
    Assert.AreEqual(15, actual.X)
    Assert.AreEqual(10, actual.Y)


[<Test>]
let ``move2 should return correct end position `` () =
    let commands =
        [| "forward 5"
           "down 5"
           "forward 8"
           "up 3"
           "down 8"
           "forward 2" |]

    let actual = move2 commands
    Assert.NotNull(actual)
    Assert.AreEqual(15, actual.X)
    Assert.AreEqual(60, actual.Y)

module Day17.Tests

open NUnit.Framework
open Program

[<Test>]
let getResult () =
    let input = "target area: x=20..30, y=-10..-5"
    let actual = getResult input
    Assert.AreEqual(45, actual)


[<Test>]
let getResult2 () =
    let input = "target area: x=20..30, y=-10..-5"
    let actual = getResult2 input
    Assert.AreEqual(112, actual)

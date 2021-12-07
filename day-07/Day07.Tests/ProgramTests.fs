module Day07.Tests

open NUnit.Framework
open Program

[<Test>]
let getMinFuel () =
    let positions = [| 16; 1; 2; 0; 4; 2; 7; 1; 2; 14 |]
    let actual = getMinFuel positions
    Assert.AreEqual(37, actual)


[<Test>]
let getMinFuel2 () =
    let positions = [| 16; 1; 2; 0; 4; 2; 7; 1; 2; 14 |]
    let actual = getMinFuel2 positions
    Assert.AreEqual(168, actual)

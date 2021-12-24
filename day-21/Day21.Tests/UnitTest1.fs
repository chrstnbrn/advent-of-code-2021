module Day21.Tests

open NUnit.Framework
open Program

[<Test>]
let getResult () =
    let input =
        [| "Player 1 starting position: 4"
           "Player 2 starting position: 8" |]

    let actual = getResult input
    Assert.AreEqual(739785, actual)

[<Test>]
let getResult2 () =
    let input =
        [| "Player 1 starting position: 4"
           "Player 2 starting position: 8" |]

    let actual = getResult2 input
    Assert.AreEqual(444356092776315L, actual)

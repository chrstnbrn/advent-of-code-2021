module Day23.Tests

open NUnit.Framework
open Program

[<Test>]
let getResult () =
    let input =
        [| "#############"
           "#...........#"
           "###B#C#B#D###"
           "  #A#D#C#A#  "
           "  #########  " |]

    let actual = getResult input
    Assert.AreEqual(12521, actual)

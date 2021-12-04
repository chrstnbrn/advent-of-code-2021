module Day03.Tests

open NUnit.Framework
open Program

let input =
    [| "00100"
       "11110"
       "10110"
       "10111"
       "10101"
       "01111"
       "00111"
       "11100"
       "10000"
       "11001"
       "00010"
       "01010" |]


[<Test>]
let getPowerConsumption () =
    let actual = getPowerConsumption input

    Assert.AreEqual(198, actual)

[<Test>]
let getLifeSupportRating () =
    let actual = getLifeSupportRating input

    Assert.AreEqual(230, actual)

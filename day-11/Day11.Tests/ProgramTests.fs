module Day11.Tests

open NUnit.Framework
open Program

let input =
    [| "5483143223"
       "2745854711"
       "5264556173"
       "6141336146"
       "6357385478"
       "4167524645"
       "2176841721"
       "6882881134"
       "4846848554"
       "5283751526" |]

[<Test>]
let getFlashes () =
    let actual = getFlashes input
    Assert.AreEqual(1656, actual)

[<Test>]
let getStep () =
    let actual = getStep input
    Assert.AreEqual(195, actual)

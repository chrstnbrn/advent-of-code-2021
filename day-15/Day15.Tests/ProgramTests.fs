module Day15.Tests

open NUnit.Framework
open Program

[<Test>]
let getLowestTotalRisk () =
    let input =
        [| "1163751742"
           "1381373672"
           "2136511328"
           "3694931569"
           "7463417111"
           "1319128137"
           "1359912421"
           "3125421639"
           "1293138521"
           "2311944581" |]

    let actual = getLowestTotalRisk input
    Assert.AreEqual(40, actual)

[<Test>]
let getLowestTotalRiskInBigMap () =
    let input =
        [| "1163751742"
           "1381373672"
           "2136511328"
           "3694931569"
           "7463417111"
           "1319128137"
           "1359912421"
           "3125421639"
           "1293138521"
           "2311944581" |]

    let actual = getLowestTotalRiskInBigMap input
    Assert.AreEqual(315, actual)

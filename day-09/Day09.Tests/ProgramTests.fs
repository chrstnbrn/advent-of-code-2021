module Day09.Tests

open NUnit.Framework
open Program

[<Test>]
let getRiskLevelSum () =
    let input =
        [| "2199943210"
           "3987894921"
           "9856789892"
           "8767896789"
           "9899965678" |]

    let actual = getRiskLevelSum input
    Assert.AreEqual(15, actual)


[<Test>]
let getBasinSizeProduct () =
    let input =
        [| "2199943210"
           "3987894921"
           "9856789892"
           "8767896789"
           "9899965678" |]

    let actual = getBasinSizeProduct input
    Assert.AreEqual(1134, actual)

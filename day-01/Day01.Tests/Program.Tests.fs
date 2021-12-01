module Day01.Tests

open NUnit.Framework
open Program

[<Test>]
let ``countDepthIncreases should return 0 for empty sequence`` () =
    let measurements = Seq.empty
    let actual = countDepthIncreases measurements
    Assert.AreEqual(0, actual)


[<Test>]
let ``countDepthIncreases should return 0 for sequence with one element`` () =
    let measurements = [| 1 |]
    let actual = countDepthIncreases measurements
    Assert.AreEqual(0, actual)

[<Test>]
let ``countDepthIncreases should return correct result for sequence with many elements`` () =
    let measurements =
        [| 199
           200
           208
           210
           200
           207
           240
           269
           260
           263 |]

    let actual = countDepthIncreases measurements
    Assert.AreEqual(7, actual)

[<Test>]
let ``countSlidingWindowDepthIncreases should return 0 for sequence with two elements`` () =
    let measurements = [| 1; 2 |]

    let actual =
        countSlidingWindowDepthIncreases measurements

    Assert.AreEqual(0, actual)

[<Test>]
let ``countSlidingWindowDepthIncreases should return correct result for sequence with many elements`` () =
    let measurements =
        [| 199
           200
           208
           210
           200
           207
           240
           269
           260
           263 |]

    let actual =
        countSlidingWindowDepthIncreases measurements

    Assert.AreEqual(5, actual)

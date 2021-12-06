module Day06.Tests

open NUnit.Framework
open Program

[<TestCase(18, 26L)>]
[<TestCase(80, 5934L)>]
[<TestCase(256, 26984457539L)>]
let getNumberOfFish ((days, expected): int * int64) =
    let input = [| 3; 4; 3; 1; 2 |]
    let actual = getNumberOfFish input days
    Assert.AreEqual(bigint expected, actual)

module Day05.Tests

open NUnit.Framework
open Program

[<Test>]
let getOverlaps () =
    let input =
        @"0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2"

    let actual = getOverlaps input

    Assert.AreEqual(5, actual)

[<Test>]
let getOverlapsIncludingDiagonals () =
    let input =
        @"0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2"

    let actual = getOverlapsIncludingDiagonals input

    Assert.AreEqual(12, actual)

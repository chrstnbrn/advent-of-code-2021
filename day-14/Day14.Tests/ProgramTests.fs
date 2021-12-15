module Day14.Tests

open NUnit.Framework
open Program

[<TestCase(0, 1L)>]
[<TestCase(10, 1588L)>]
[<TestCase(40, 2188189693529L)>]
let getResult steps expected =
    let input =
        [| "NNCB"
           ""
           "CH -> B"
           "HH -> N"
           "CB -> H"
           "NH -> C"
           "HB -> C"
           "HC -> B"
           "HN -> C"
           "NN -> C"
           "BH -> H"
           "NC -> B"
           "NB -> B"
           "BN -> B"
           "BB -> N"
           "BC -> B"
           "CC -> N"
           "CN -> C" |]

    let actual = getResult input steps
    Assert.AreEqual(expected, actual)

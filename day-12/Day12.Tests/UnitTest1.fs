module Day12.Tests

open NUnit.Framework
open Program

[<Test>]
let getNumberOfPathsSmall () =
    let input =
        [| "start-A"
           "start-b"
           "A-c"
           "A-b"
           "b-d"
           "A-end"
           "b-end" |]

    let actual = getNumberOfPaths input
    Assert.AreEqual(10, actual)


[<Test>]
let getNumberOfPathsLarger () =
    let input =
        [| "dc-end"
           "HN-start"
           "start-kj"
           "dc-start"
           "dc-HN"
           "LN-dc"
           "HN-end"
           "kj-sa"
           "kj-HN"
           "kj-dc" |]

    let actual = getNumberOfPaths input
    Assert.AreEqual(19, actual)

[<Test>]
let getNumberOfPathsEvenLarger () =
    let input =
        [| "fs-end"
           "he-DX"
           "fs-he"
           "start-DX"
           "pj-DX"
           "end-zg"
           "zg-sl"
           "zg-pj"
           "pj-he"
           "RW-he"
           "fs-DX"
           "pj-RW"
           "zg-RW"
           "start-pj"
           "he-WI"
           "zg-he"
           "pj-fs"
           "start-RW" |]

    let actual = getNumberOfPaths input
    Assert.AreEqual(226, actual)

[<Test>]
let getNumberOfPathsSmall2 () =
    let input =
        [| "start-A"
           "start-b"
           "A-c"
           "A-b"
           "b-d"
           "A-end"
           "b-end" |]

    let actual = getNumberOfPaths2 input
    Assert.AreEqual(36, actual)


[<Test>]
let getNumberOfPathsLarger2 () =
    let input =
        [| "dc-end"
           "HN-start"
           "start-kj"
           "dc-start"
           "dc-HN"
           "LN-dc"
           "HN-end"
           "kj-sa"
           "kj-HN"
           "kj-dc" |]

    let actual = getNumberOfPaths2 input
    Assert.AreEqual(103, actual)

[<Test>]
let getNumberOfPathsEvenLarger2 () =
    let input =
        [| "fs-end"
           "he-DX"
           "fs-he"
           "start-DX"
           "pj-DX"
           "end-zg"
           "zg-sl"
           "zg-pj"
           "pj-he"
           "RW-he"
           "fs-DX"
           "pj-RW"
           "zg-RW"
           "start-pj"
           "he-WI"
           "zg-he"
           "pj-fs"
           "start-RW" |]

    let actual = getNumberOfPaths2 input
    Assert.AreEqual(3509, actual)

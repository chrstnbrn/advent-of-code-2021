module Day18.Tests

open NUnit.Framework
open Program

[<TestCase("[[[[[9,8],1],2],3],4]", "[[[[0,9],2],3],4]")>]
[<TestCase("[7,[6,[5,[4,[3,2]]]]]", "[7,[6,[5,[7,0]]]]")>]
[<TestCase("[[6,[5,[4,[3,2]]]],1]", "[[6,[5,[7,0]]],3]")>]
[<TestCase("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]", "[[3,[2,[8,0]]],[9,[5,[7,0]]]]")>]
[<TestCase("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]", "[[3,[2,[8,0]]],[9,[5,[7,0]]]]")>]
[<TestCase("[10,1]", "[[5,5],1]")>]
[<TestCase("[11,1]", "[[5,6],1]")>]
[<TestCase("[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]", "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]")>]
let reduce number expected =
    let actual = reduce number
    Assert.AreEqual(expected, actual)

[<TestCase("[[[[4,3],4],4],[7,[[8,4],9]]]", "[1,1]", "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]")>]
[<TestCase("[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]",
           "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]",
           "[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]")>]
let add a b expected =
    let actual = add a b
    Assert.AreEqual(expected, actual)

[<Test>]
let addMany1 () =
    let input = [ "[1,1]"; "[2,2]"; "[3,3]"; "[4,4]" ]
    let actual = addMany input
    Assert.AreEqual("[[[[1,1],[2,2]],[3,3]],[4,4]]", actual)

[<Test>]
let addMany2 () =
    let input =
        [ "[1,1]"
          "[2,2]"
          "[3,3]"
          "[4,4]"
          "[5,5]" ]

    let actual = addMany input
    Assert.AreEqual("[[[[3,0],[5,3]],[4,4]],[5,5]]", actual)


[<Test>]
let addMany3 () =
    let input =
        [ "[1,1]"
          "[2,2]"
          "[3,3]"
          "[4,4]"
          "[5,5]"
          "[6,6]" ]

    let actual = addMany input
    Assert.AreEqual("[[[[5,0],[7,4]],[5,5]],[6,6]]", actual)

[<Test>]
let addMany4 () =
    let input =
        [ "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]"
          "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]"
          "[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]"
          "[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]"
          "[7,[5,[[3,8],[1,4]]]]"
          "[[2,[2,2]],[8,[8,1]]]"
          "[2,9]"
          "[1,[[[9,3],9],[[9,0],[0,7]]]]"
          "[[[5,[7,4]],7],1]"
          "[[[[4,2],2],6],[8,7]]" ]

    let actual = addMany input
    Assert.AreEqual("[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]", actual)

[<TestCase("[9,1]", 29)>]
[<TestCase("[[9,1],[1,9]]", 129)>]
[<TestCase("[[1,2],[[3,4],5]]", 143)>]
[<TestCase("[[[[0,7],4],[[7,8],[6,0]]],[8,1]]", 1384)>]
[<TestCase("[[[[1,1],[2,2]],[3,3]],[4,4]]", 445)>]
[<TestCase("[[[[3,0],[5,3]],[4,4]],[5,5]]", 791)>]
[<TestCase("[[[[5,0],[7,4]],[5,5]],[6,6]]", 1137)>]
[<TestCase("[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]", 3488)>]
let getMagnitude input expected =
    let actual = getMagnitude input
    Assert.AreEqual(expected, actual)

[<Test>]
let getMagnitudeOfSum () =
    let input =
        [| "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]"
           "[[[5,[2,8]],4],[5,[[9,9],0]]]"
           "[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]"
           "[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]"
           "[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]"
           "[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]"
           "[[[[5,4],[7,7]],8],[[8,3],8]]"
           "[[9,3],[[9,9],[6,[4,9]]]]"
           "[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]"
           "[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]" |]

    let actual = getMagnitudeOfSum input
    Assert.AreEqual(4140, actual)

[<Test>]
let getLargestMagnitude () =
    let input =
        [| "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]"
           "[[[5,[2,8]],4],[5,[[9,9],0]]]"
           "[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]"
           "[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]"
           "[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]"
           "[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]"
           "[[[[5,4],[7,7]],8],[[8,3],8]]"
           "[[9,3],[[9,9],[6,[4,9]]]]"
           "[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]"
           "[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]" |]

    let actual = getLargestMagnitude input
    Assert.AreEqual(3993, actual)

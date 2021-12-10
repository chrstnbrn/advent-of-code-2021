module Day10.Tests

open NUnit.Framework
open Program

let input =
    [| "[({(<(())[]>[[{[]{<()<>>"
       "[(()[<>])]({[<{<<[]>>("
       "{([(<{}[<>[]}>{[]{[(<()>"
       "(((({<>}<{<{<>}{[]{[]{}"
       "[[<[([]))<([[{}[[()]]]"
       "[{[{({}]{}}([{[{{{}}([]"
       "{<[[]]>}<{[{[{[]{()[[[]"
       "[<(<(<(<{}))><([]([]()"
       "<{([([[(<>()){}]>(<<{{"
       "<{([{{}}[<[[[<>{}]]]>[]]" |]

[<Test>]
let getResult () =
    let actual = getTotalSyntaxErrorScore input
    Assert.AreEqual(26397, actual)

[<Test>]
let getResult2 () =
    let actual = getMiddleScore input
    Assert.AreEqual(288957, actual)

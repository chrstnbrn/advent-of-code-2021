open System.IO

let getFirstCorrupedCharacter (line: string) =
    let mutable expectedClosings = []
    let mutable corruptedCharacter = None
    let mutable i = 0

    while Option.isNone corruptedCharacter
          && i < line.Length do
        let ch = line.[i]

        match ch with
        | '(' -> expectedClosings <- ')' :: expectedClosings
        | '[' -> expectedClosings <- ']' :: expectedClosings
        | '{' -> expectedClosings <- '}' :: expectedClosings
        | '<' -> expectedClosings <- '>' :: expectedClosings
        | _ when (ch = List.head expectedClosings) -> expectedClosings <- expectedClosings.Tail
        | _ -> corruptedCharacter <- Some ch

        i <- i + 1

    corruptedCharacter

let getCompletionCharacters (line: string) =
    let mutable expectedClosings = []
    let mutable corruptedCharacter = None
    let mutable i = 0

    while Option.isNone corruptedCharacter
          && i < line.Length do
        let ch = line.[i]

        match ch with
        | '(' -> expectedClosings <- ')' :: expectedClosings
        | '[' -> expectedClosings <- ']' :: expectedClosings
        | '{' -> expectedClosings <- '}' :: expectedClosings
        | '<' -> expectedClosings <- '>' :: expectedClosings
        | _ when (ch = List.head expectedClosings) -> expectedClosings <- expectedClosings.Tail
        | _ -> corruptedCharacter <- Some ch

        i <- i + 1

    if Option.isSome corruptedCharacter then
        None
    else
        Some expectedClosings

let getSyntaxErrorScore ch =
    match ch with
    | ')' -> 3
    | ']' -> 57
    | '}' -> 1197
    | '>' -> 25137
    | _ -> 0

let getCompletionScore characters =
    let getPointValue ch =
        match ch with
        | ')' -> 1
        | ']' -> 2
        | '}' -> 3
        | '>' -> 4
        | _ -> 0

    characters
    |> Seq.fold (fun totalScore ch -> (totalScore * 5L) + (getPointValue ch |> int64)) 0L

let getMedian x =
    let sorted = x |> Seq.sort |> Seq.toArray
    sorted.[sorted.Length / 2]

let getTotalSyntaxErrorScore input =
    input
    |> Seq.choose getFirstCorrupedCharacter
    |> Seq.sumBy getSyntaxErrorScore

let getMiddleScore input =
    input
    |> Seq.choose getCompletionCharacters
    |> Seq.map getCompletionScore
    |> getMedian

let input = File.ReadAllLines "./input.txt"

let resultPart1 = getTotalSyntaxErrorScore input
printfn "Result Part 1: %d" resultPart1

let resultPart2 = getMiddleScore input
printfn "Result Part 2: %d" resultPart2

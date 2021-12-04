open System
open System.IO

let getRows arr =
    let height = arr |> Array2D.length1

    [ 0 .. height - 1 ]
    |> Seq.map (fun i -> arr[i, *])

let getColumns arr =
    let width = arr |> Array2D.length2

    [ 0 .. width - 1 ] |> Seq.map (fun i -> arr[*, i])

let hasBingo drawnNumbers values =
    values
    |> Seq.map Set
    |> Seq.map (fun x -> Set.difference x drawnNumbers)
    |> Seq.exists Set.isEmpty

let hasBoardWon drawnNumbers board =
    let drawnNumbersSet = Set drawnNumbers
    let hasBingoRow = board |> getRows |> hasBingo drawnNumbersSet
    let hasBingoColumn = board |> getColumns |> hasBingo drawnNumbersSet
    hasBingoRow || hasBingoColumn

let getWinningScore drawnNumbers board =
    let drawnNumbersSet = Set drawnNumbers
    let boardNumbers = board |> Seq.cast<int> |> Set
    let unmarkedNumbers = Set.difference boardNumbers drawnNumbersSet
    Seq.sum unmarkedNumbers * (Seq.last drawnNumbers)

let rec getFinalScoreRec drawnNumbers count boards =
    let numbers = drawnNumbers |> Array.take count
    let winningBoard = boards |> Seq.tryFind (hasBoardWon numbers)

    if Option.isSome winningBoard then
        getWinningScore numbers winningBoard.Value
    else
        getFinalScoreRec drawnNumbers (count + 1) boards

let getFinalScore drawnNumbers boards = getFinalScoreRec drawnNumbers 1 boards

let rec getFinalScoreOfLastWinningBoardRec drawnNumbers count boards =
    let numbers = drawnNumbers |> Array.take count

    let remainingBoards =
        boards
        |> Seq.filter (fun board -> hasBoardWon numbers board = false)


    if Seq.isEmpty remainingBoards then
        boards |> Seq.head |> getWinningScore numbers
    else
        getFinalScoreOfLastWinningBoardRec drawnNumbers (count + 1) remainingBoards

let getFinalScoreOfLastWinningBoard drawnNumbers boards =
    getFinalScoreOfLastWinningBoardRec drawnNumbers 1 boards

let input = File.ReadAllText "./input.txt"
let blocks = input.Split(Environment.NewLine + Environment.NewLine)

let drawnNumbers =
    blocks
    |> Array.head
    |> (fun x -> x.Split ",")
    |> Array.map int

let parseBoard (block: string) =
    block.Split Environment.NewLine
    |> Array.map (fun line ->
        line.Trim().Split()
        |> Array.filter ((<>) "")
        |> Array.map int)
    |> array2D

let boards = blocks |> Array.tail |> Array.map parseBoard

let resultPart1 = getFinalScore drawnNumbers boards
printfn "Result Part 1: %d" resultPart1

let resultPart2 = getFinalScoreOfLastWinningBoard drawnNumbers boards
printfn "Result Part 2: %d" resultPart2

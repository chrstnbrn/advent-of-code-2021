open System
open System.IO
open System.Text

let parseInput (input: string array) =
    let coordinates =
        input
        |> Array.takeWhile ((<>) "")
        |> Array.map (fun x ->
            x.Split(",")
            |> Array.map int
            |> (fun x -> (x.[0], x.[1])))
        |> Set

    let foldInstructions =
        input
        |> Array.filter (fun x -> x.StartsWith("fold along "))
        |> Array.map (fun x -> x.Replace("fold along ", ""))
        |> Array.map (fun x -> x.Split("=") |> (fun x -> (x.[0], int x.[1])))

    (coordinates, foldInstructions)

let foldX (coordinates: (int * int) Set) foldPosition =
    let maxX = coordinates |> Seq.map fst |> Seq.max

    coordinates
    |> Seq.map (fun (x, y) -> (if x < foldPosition then x else maxX - x), y)
    |> Set

let foldY (coordinates: (int * int) Set) foldPosition =
    let maxY = coordinates |> Seq.map snd |> Seq.max

    coordinates
    |> Seq.map (fun (x, y) -> (x, (if y < foldPosition then y else maxY - y)))
    |> Set

let fold coordinates instruction =
    let direction, position = instruction
    let foldFn = if direction = "x" then foldX else foldY
    foldFn coordinates position

let toText coordinates =
    let maxX = coordinates |> Seq.map fst |> Seq.max
    let maxY = coordinates |> Seq.map snd |> Seq.max
    let sb = new StringBuilder()

    for j in [ 0..maxY ] do
        for i in [ 0..maxX ] do
            let sign =
                if coordinates |> Set.contains (i, j) then
                    "█"
                else
                    " "

            sb.Append(sign) |> ignore

        sb.AppendLine() |> ignore

    sb.ToString()

let getVisibleDots input =
    let coordinates, foldInstructions = parseInput input

    fold coordinates foldInstructions.[0]
    |> Seq.length

let foldAll input =
    let coordinates, foldInstructions = parseInput input

    foldInstructions
    |> Seq.fold fold coordinates
    |> toText

let input = File.ReadAllLines "./input.txt"
let resultPart1 = getVisibleDots input
printfn "Result Part 1: %d" resultPart1

let resultPart2 = foldAll input
printfn "Result Part 2: %s%s" Environment.NewLine resultPart2

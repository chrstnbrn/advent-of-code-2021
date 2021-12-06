open System.IO

let simulateDay (fish: Map<int, bigint>) _ =
    [ 0..8 ]
    |> Seq.map (fun i ->
        (i,
         match i with
         | 8 -> fish.[0]
         | 6 -> fish.[i + 1] + fish.[0]
         | _ -> fish.[i + 1]))
    |> Map

let getNumberOfFish (initialState: int array) (days: int) =
    let count predicate x =
        x |> Seq.filter predicate |> Seq.length |> bigint

    let fish =
        [ 0..8 ]
        |> Seq.map (fun i -> (i, initialState |> count ((=) i)))
        |> Map

    [ 1..days ]
    |> Seq.fold simulateDay fish
    |> Map.values
    |> Seq.sum

let input = File.ReadAllText "./input.txt"
let initialState = input.Split(",") |> Array.map int

let resultPart1 = getNumberOfFish initialState 80
printfn "Result Part 1: %A" resultPart1

let stopWatch = System.Diagnostics.Stopwatch.StartNew()
let resultPart2 = getNumberOfFish initialState 256
printfn "Result Part 2: %A" resultPart2

open System.IO

let parse (input: string array) =
    let template =
        input
        |> Array.head
        |> Seq.cast<char>
        |> Seq.toArray

    let rules =
        input
        |> Array.filter (fun x -> x.Contains("->"))
        |> Array.map (fun x ->
            x.Split(" -> ")
            |> (fun p -> ((p.[0].[0], p.[0].[1]), p.[1].[0])))
        |> Map

    (template, rules)

let getPairCounts (pairCounts: Map<char * char, int64>) (rules: Map<char * char, char>) =
    pairCounts
    |> Seq.collect (fun x ->
        [ ((fst x.Key, rules.[x.Key]), x.Value)
          ((rules.[x.Key], snd x.Key), x.Value) ])
    |> Seq.groupBy fst
    |> Seq.map (fun (x, counts) -> (x, counts |> Seq.sumBy snd))
    |> Map

let getResult input steps =
    let template, rules = parse input

    let initialPairCounts =
        template
        |> Seq.pairwise
        |> Seq.countBy id
        |> Seq.map (fun (pair, count) -> (pair, int64 count))
        |> Map

    let pairCounts =
        [ 1..steps ]
        |> Seq.fold (fun t _ -> getPairCounts t rules) initialPairCounts

    let counts =
        pairCounts
        |> Map.keys
        |> Seq.map (fun (a, b) -> (a, pairCounts.[a, b]))
        |> Seq.append [ (Seq.last template, 1) ]
        |> Seq.groupBy fst
        |> Seq.map (fun (_, counts) -> counts |> Seq.sumBy snd)

    (Seq.max counts - Seq.min counts)

let input = File.ReadAllLines "./input.txt"

let resultPart1 = getResult input 10
printfn "Result Part 1: %d" resultPart1

let resultPart2 = getResult input 40
printfn "Result Part 2: %d" resultPart2

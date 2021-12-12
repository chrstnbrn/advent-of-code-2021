open System.IO

let getNeighborMap (input: string array) =
    input
    |> Array.map (fun x -> x.Split("-"))
    |> Seq.collect (fun x -> [ (x.[0], x.[1]); (x.[1], x.[0]) ])
    |> Seq.groupBy fst
    |> Seq.map (fun (from, tos) -> (from, tos |> Seq.map snd |> Seq.toArray))
    |> Map

let isBigCave (cave: string) = cave.ToUpper() = cave

let isSmallCave (cave: string) =
    cave <> "start"
    && cave <> "end"
    && not (isBigCave cave)

let rec findPath (neighborMap: Map<string, string []>) (currentPath: string list) =
    let current = List.head currentPath

    if current = "end" then
        Seq.singleton currentPath
    else
        neighborMap.[current]
        |> Seq.filter (fun c -> not (List.contains c currentPath) || isBigCave c)
        |> Seq.collect (fun c -> findPath neighborMap (c :: currentPath))

let rec findPath2 (neighborMap: Map<string, string []>) (currentPath: string list) =
    let current = List.head currentPath

    if current = "end" then
        Seq.singleton currentPath
    else
        let hasVisitedSmallCaveTwice =
            currentPath
            |> Seq.filter isSmallCave
            |> Seq.countBy id
            |> Seq.exists (fun (_, count) -> count > 1)

        neighborMap.[current]
        |> Seq.filter (fun c ->
            not (List.contains c currentPath)
            || isBigCave c
            || (isSmallCave c && not hasVisitedSmallCaveTwice))
        |> Seq.collect (fun c -> findPath2 neighborMap (c :: currentPath))

let getNumberOfPaths input =
    let neighborMap = getNeighborMap input

    findPath neighborMap [ "start" ] |> Seq.length

let getNumberOfPaths2 input =
    let neighborMap = getNeighborMap input

    findPath2 neighborMap [ "start" ] |> Seq.length

let input = File.ReadAllLines "./input.txt"

let resultPart1 = getNumberOfPaths input
printfn "Result Part 1: %d" resultPart1

let stopWatch = new System.Diagnostics.Stopwatch()
stopWatch.Start()
let resultPart2 = getNumberOfPaths2 input
stopWatch.Stop()
printfn "Result Part 2: %d in %dms" resultPart2 stopWatch.ElapsedMilliseconds

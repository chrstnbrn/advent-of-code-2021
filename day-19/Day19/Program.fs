open System
open System.IO

let parse (input: string) =
    let blocks = input.Split(Environment.NewLine + Environment.NewLine)

    blocks
    |> Array.map (fun b ->
        b.Split(Environment.NewLine)
        |> Array.skip 1
        |> Array.map (fun line ->
            let x = line.Split(",")
            (int x.[0], int x.[1], int x.[2])))


let transformCoordinates coordinates newBase =
    let (b1, b2, b3) = newBase

    coordinates
    |> Array.map (fun (c1, c2, c3) -> (c1 - b1, c2 - b2, c3 - b3))

let getRotations () =
    [ fun (x, y, z) -> (x, y, z)
      fun (x, y, z) -> (y, z, x)
      fun (x, y, z) -> (z, x, y)
      fun (x, y, z) -> (-x, z, y)
      fun (x, y, z) -> (z, y, -x)
      fun (x, y, z) -> (y, -x, z)
      fun (x, y, z) -> (x, -z, y)
      fun (x, y, z) -> (-z, y, x)
      fun (x, y, z) -> (y, x, -z)
      fun (x, y, z) -> (-x, y, -z)
      fun (x, y, z) -> (y, -z, -x)
      fun (x, y, z) -> (-z, -x, y)
      fun (x, y, z) -> (x, -y, -z)
      fun (x, y, z) -> (-y, -z, x)
      fun (x, y, z) -> (-z, x, -y)
      fun (x, y, z) -> (-x, -z, -y)
      fun (x, y, z) -> (-z, -y, -x)
      fun (x, y, z) -> (-y, -x, -z)
      fun (x, y, z) -> (x, z, -y)
      fun (x, y, z) -> (z, -y, x)
      fun (x, y, z) -> (-y, x, z)
      fun (x, y, z) -> (-x, -y, z)
      fun (x, y, z) -> (-y, z, -x)
      fun (x, y, z) -> (z, -x, -y) ]

let getAllRotations coordinates =
    seq {
        for c in coordinates do
            let transformedCoordinates = transformCoordinates coordinates c

            for rotation in getRotations () do
                let result = transformedCoordinates |> Array.map rotation

                let (c1, c2, c3) = c
                let scannerPosition = rotation (-c1, -c2, -c3)
                yield (scannerPosition, Set result)
    }

let decode scanner scanner0 =
    let all0Combinations =
        scanner0
        |> Seq.map (fun s -> (s, Set(transformCoordinates scanner0 s)))

    let x =
        seq {
            for c in scanner do
                let transformedCoordinates = transformCoordinates scanner c

                for rotation in getRotations () do
                    let result = transformedCoordinates |> Array.map rotation

                    let (c1, c2, c3) = c
                    let scannerPosition = rotation (-c1, -c2, -c3)
                    yield (scannerPosition, Set result)
        }

    let result =
        Seq.allPairs all0Combinations x
        |> Seq.tryFind (fun ((s, zeroCombination), (scannerPosition, values)) ->
            Set.intersect zeroCombination values
            |> (fun intersection -> intersection.Count >= 12))

    result
    |> Option.map (fun ((s, zeroCombination), (scannerPosition, values)) ->
        let (t1, t2, t3) = s

        let scannerPosition =
            transformCoordinates [| scannerPosition |] (-t1, -t2, -t3)
            |> Array.head

        let values = transformCoordinates (values |> Set.toArray) (-t1, -t2, -t3)
        (scannerPosition, values))

let getManhattanDistance ((x1, y1, z1): int * int * int) ((x2, y2, z2): int * int * int) =
    Math.Abs(x1 - x2)
    + Math.Abs(y1 - y2)
    + Math.Abs(z1 - z2)

let identify scanners =
    let mutable identified = [| ((0, 0, 0), Array.head scanners) |]
    let mutable unidentified = Array.tail scanners

    while unidentified.Length > 0 do
        printfn "===== Number of unidentified scanners: %d =====" unidentified.Length

        for identifiedScanner in identified do
            for scanner in unidentified do
                let result = decode scanner (snd identifiedScanner)

                if result.IsSome then
                    printfn "Identified %A" result.Value
                    identified <- Array.append identified [| result.Value |]
                    unidentified <- unidentified |> Array.filter ((<>) scanner)

    identified

let getResult input =
    input
    |> parse
    |> identify
    |> Array.map (snd >> Set)
    |> Set.unionMany
    |> Seq.length

let getMaxDistance input =
    let scannerPositions =
        input
        |> parse
        |> identify
        |> Seq.map fst
        |> Seq.toArray

    Seq.allPairs scannerPositions scannerPositions
    |> Seq.map (fun (a, b) -> getManhattanDistance a b)
    |> Seq.max


let input = File.ReadAllText "./input.txt"

let resultPart1 = getResult input
printfn "Result Part 1: %d" resultPart1

let stopWatch = Diagnostics.Stopwatch()
stopWatch.Start()
let resultPart2 = getMaxDistance input
stopWatch.Stop()
printfn "Result Part 2: %d in %d" resultPart2 stopWatch.ElapsedMilliseconds

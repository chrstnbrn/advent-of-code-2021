open System
open System.IO
open QuikGraph
open QuikGraph.Algorithms

let parseRiskLevelMap (input: string array) =
    input |> array2D |> Array2D.map (string >> int)

let getNeighbors map (i, j) =
    let m, n = (Array2D.length1 map, Array2D.length2 map)

    [ (i - 1, j)
      (i + 1, j)
      (i, j - 1)
      (i, j + 1) ]
    |> List.filter (fun (x, y) -> x >= 0 && x < m && y >= 0 && y < n)

let getEdges map node =
    node
    |> getNeighbors map
    |> Seq.map (fun neighbor -> Edge<int * int>(node, neighbor))

let getLowestRisk start goal map =
    let edges =
        map
        |> Array2D.mapi (fun i j _ -> (i, j))
        |> Seq.cast<int * int>
        |> Seq.collect (getEdges map)

    let graph = edges.ToAdjacencyGraph()

    let edgeWeights =
        new Func<Edge<int * int>, double>(fun (e) -> double map.[fst e.Source, snd e.Source])

    let tryGetPath = graph.ShortestPathsDijkstra(edgeWeights, start)

    tryGetPath.Invoke goal
    |> snd
    |> Seq.map (fun edge -> edge.Target)
    |> Seq.sumBy (fun (i, j) -> map.[i, j])

let toBigMap map =
    let m, n = (Array2D.length1 map, Array2D.length2 map)

    Array2D.init (m * 5) (n * 5) (fun i j ->
        let value = map.[i % m, j % n] + i / m + j / n
        if value > 9 then value % 9 else value)

let getLowestTotalRisk input =
    let map = parseRiskLevelMap input

    map
    |> getLowestRisk (0, 0) (Array2D.length1 map - 1, Array2D.length2 map - 1)

let getLowestTotalRiskInBigMap input =
    let map = parseRiskLevelMap input |> toBigMap

    map
    |> getLowestRisk (0, 0) (Array2D.length1 map - 1, Array2D.length2 map - 1)

let input = File.ReadAllLines "./input.txt"

let resultPart1 = getLowestTotalRisk input
printfn "Result Part 1: %d" resultPart1

let resultPart2 = getLowestTotalRiskInBigMap input
printfn "Result Part 2: %d" resultPart2

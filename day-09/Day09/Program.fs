open System.IO

type Heightmap = int [,]
type Coordinates = int * int

let parseHeightmap (input: string array) =
    Array2D.init input.Length input.[0].Length (fun i j -> input.[i].[j] |> string |> int)

let getValue (heightmap: Heightmap) (coordinate: Coordinates) =
    heightmap.[fst coordinate, snd coordinate]

let getNeighborCoordinates coordinates (A: 'a [,]) =
    let i, j = coordinates
    let length1, length2 = (Array2D.length1 A, Array2D.length2 A)

    [ if i > 0 then yield (i - 1, j)
      if i < length1 - 1 then yield (i + 1, j)
      if j > 0 then yield (i, j - 1)
      if j < length2 - 1 then yield (i, j + 1) ]

let isLowpoint heightmap coordinates =
    let value = getValue heightmap coordinates

    heightmap
    |> getNeighborCoordinates coordinates
    |> Seq.map (getValue heightmap)
    |> Seq.forall (fun x -> x > value)

let getLowPoints heightmap =
    heightmap
    |> Array2D.mapi (fun i j _ -> (i, j))
    |> Seq.cast<int * int>
    |> Seq.filter (isLowpoint heightmap)

let getRiskLevel heightmap coordinate = getValue heightmap coordinate + 1

let getCoordinatesInBasin (coordinate: Coordinates) heightmap =
    let mutable visited = Set.empty
    let mutable active = [ coordinate ]

    while not (List.isEmpty active) do
        let current = List.head active
        active <- List.tail active
        visited <- Set.add current visited

        let neighborsInBasin =
            getNeighborCoordinates current heightmap
            |> Seq.filter (fun x ->
                getValue heightmap x <> 9
                && not (visited |> Set.contains x))

        neighborsInBasin
        |> Seq.iter (fun x -> (active <- x :: active))

    visited

let getBasinSize heightmap lowPoint =
    getCoordinatesInBasin lowPoint heightmap
    |> Seq.length

let getRiskLevelSum (input: string array) =
    let heightmap = parseHeightmap input

    heightmap
    |> getLowPoints
    |> Seq.sumBy (getRiskLevel heightmap)

let getBasinSizeProduct (input: string array) =
    let heightmap = parseHeightmap input

    heightmap
    |> getLowPoints
    |> Seq.map (getBasinSize heightmap)
    |> Seq.sortDescending
    |> Seq.take 3
    |> Seq.reduce (*)

let input = File.ReadAllLines "./input.txt"

let resultPart1 = getRiskLevelSum input
printfn "Result Part 1: %d" resultPart1

let resultPart2 = getBasinSizeProduct input
printfn "Result Part 2: %d" resultPart2

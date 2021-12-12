open System.IO

let parseInput lines =
    lines |> array2D |> Array2D.map (string >> int)

let getNeighbors (i, j) arr =
    [ (i - 1, j - 1)
      (i - 1, j)
      (i - 1, j + 1)
      (i, j - 1)
      (i, j + 1)
      (i + 1, j - 1)
      (i + 1, j)
      (i + 1, j + 1) ]
    |> Seq.filter (fun (x, y) ->
        x >= 0
        && x < Array2D.length1 arr
        && y >= 0
        && y < Array2D.length2 arr)

let increaseNeighbors grid coordinates =
    let neighbors = getNeighbors coordinates grid |> Set

    grid
    |> Array2D.mapi (fun i j energyLevel ->
        if neighbors |> Set.contains (i, j) then
            energyLevel + 1
        else
            energyLevel)

let rec flash flashed grid =
    let flashing =
        grid
        |> Array2D.mapi (fun i j _ -> (i, j))
        |> Seq.cast<int * int>
        |> Seq.filter (fun (i, j) -> grid.[i, j] > 9)
        |> Set

    let newFlashing = Set.difference flashing flashed

    if newFlashing.Count > 0 then
        newFlashing
        |> Seq.fold increaseNeighbors grid
        |> flash flashing
    else
        grid

let simulateRound grid =
    grid
    |> Array2D.map ((+) 1)
    |> flash Set.empty
    |> Array2D.map (fun x -> if x > 9 then 0 else x)

let getFlashes input =
    let grid = parseInput input

    [ 1 .. 100 ]
    |> Seq.scan (fun state _ -> simulateRound state) grid
    |> Seq.sumBy (Seq.cast<int> >> Seq.filter ((=) 0) >> Seq.length)

let getStep input =
    let grid = parseInput input

    Seq.initInfinite id
    |> Seq.scan (fun state _ -> simulateRound state) grid
    |> Seq.findIndex (Seq.cast<int> >> Seq.forall ((=) 0))

let input = File.ReadAllLines "./input.txt"

let resultPart1 = getFlashes input
printfn "Result Part 1: %d" resultPart1

let resultPart2 = getStep input
printfn "Result Part 2: %d" resultPart2

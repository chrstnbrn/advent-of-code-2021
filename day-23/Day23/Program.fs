open System.IO
open QuikGraph

let parse input = array2D input

let getNeighbors map (i, j) =
    let m, n = (Array2D.length1 map, Array2D.length2 map)

    [ (i - 1, j)
      (i + 1, j)
      (i, j - 1)
      (i, j + 1) ]
    |> List.filter (fun (x, y) -> x >= 0 && x < m && y >= 0 && y < n)

let getTargets x =
    match x with
    | 'A' -> [ (2, 3); (3, 3) ]
    | 'B' -> [ (2, 5); (3, 5) ]
    | 'C' -> [ (2, 7); (3, 7) ]
    | 'D' -> [ (2, 9); (3, 9) ]

let getFirstRowFields () =
    [ (1, 1)
      (1, 2)
      (1, 4)
      (1, 6)
      (1, 8)
      (1, 10)
      (1, 11) ]

let getValidMoveTargets x =
    let generalFields =
        [ (1, 1)
          (1, 2)
          (1, 4)
          (1, 6)
          (1, 8)
          (1, 10)
          (1, 11) ]

    let targets = getTargets x
    List.append generalFields targets

let getPathX x1 x2 y =
    printfn "Path x for %d %d" x1 x2
    let step = if x1 < x2 then 1 else -1
    [ x1..step..x2 ] |> List.map (fun x -> (x, y))

let getPathY x y1 y2 =
    printfn "Path y for %d %d" y1 y2
    let step = if y1 < y2 then 1 else -1
    [ y1..step..y2 ] |> List.map (fun y -> (x, y))

let getPath (x1, y1) (x2, y2) =
    if y1 = y2 then
        getPathX x1 x2 y1 |> Seq.toArray
    else if x1 = 1 then
        Set.union (Set(getPathX x1 1 y1)) (Set(getPathY 1 y1 y2))
        |> Set.remove (x1, y1)
        |> Set.toArray
    else if x2 = 1 then
        Set.union (Set(getPathX x1 1 y1)) (Set(getPathY 1 y1 y2))
        |> Set.remove (x1, y1)
        |> Set.toArray
    else
        Set.unionMany [ Set(getPathX x1 1 y1)
                        Set(getPathY 1 y1 y2)
                        Set(getPathX x1 x2 y2) ]
        |> Set.remove (x1, y1)
        |> Set.toArray

let canMoveTo (map: char [,]) (x1, y1) (x2, y2) =
    if (x1, y1) = (x2, y2) then
        false
    else if x1 = 1 && x2 = 1 then
        false
    else
        let path = getPath (x1, y1) (x2, y2)
        printfn "Path from %d,%d to %d,%d: %A" x1 y1 x2 y2 path

        path
        |> Seq.map (fun (i, j) -> map.[i, j])
        |> Seq.forall ((=) '.')

let getPossibleMoves (map: char [,]) (i, j) =
    printfn "Get moves for %d, %d" i j
    let targets = getTargets map.[i, j]

    if List.contains (i, j) targets then
        []
    else
        let canEnterTarget =
            targets
            |> List.forall (fun (x, y) -> map.[x, y] = '.' || map.[x, y] = map.[i, j])

        if canEnterTarget then
            let reachableTargets = targets |> Seq.filter (canMoveTo map (i, j))

            if Seq.isEmpty reachableTargets then
                []
            else
                reachableTargets
                |> Seq.maxBy snd
                |> List.singleton
        else
            printfn "Here"

            getFirstRowFields ()
            |> List.filter (canMoveTo map (i, j))

let getAmphipodPositions map =
    map
    |> Array2D.mapi (fun i j x -> (i, j))
    |> Seq.cast<int * int>
    |> Seq.filter (fun (i, j) ->
        let x = map.[i, j]
        x = 'A' || x = 'B' || x = 'C' || x = 'D')

let doMove (map: char [,]) a b =
    map
    |> Array2D.mapi (fun i j x ->
        if (i, j) = a then
            '.'
        else if (i, j) = b then
            map.[fst a, snd a]
        else
            x)

let rec getAllPossibleMoves map =
    let moves =
        map
        |> getAmphipodPositions
        |> Seq.collect (fun a ->
            a
            |> getPossibleMoves map
            |> Seq.map (fun t -> (a, t)))

    printfn "Moves %A" moves

    moves
    |> Seq.toList
    |> List.collect (fun move ->
        let newMap = doMove map (fst move) (snd move)
        let next = getAllPossibleMoves newMap
        move :: next)

let getResult input =
    let map = parse input
    let moves = getAllPossibleMoves map

    // let winningMoves = moves |> Seq.filter (fun )
    moves.Length

let input = File.ReadAllLines("./input.txt")

let resultPart1 = getResult input
printfn "Result Part 1: %d" resultPart1

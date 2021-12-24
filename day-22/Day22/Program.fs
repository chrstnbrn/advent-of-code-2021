open System.IO

type Cube =
    { X1: int
      X2: int
      Y1: int
      Y2: int
      Z1: int
      Z2: int }
    member this.Values =
        [| this.X1
           this.X2
           this.Y1
           this.Y2
           this.Z1
           this.Z2 |]

    member this.Volume =
        (int64 this.X2 - int64 this.X1 + 1L)
        * (int64 this.Y2 - int64 this.Y1 + 1L)
        * (int64 this.Z2 - int64 this.Z1 + 1L)

    member cube.Intersect(other: Cube) =
        if cube.X2 < other.X1
           || other.X2 < cube.X1
           || cube.Y2 < other.Y1
           || other.Y2 < cube.Y1
           || cube.Z2 < other.Z1
           || other.Z2 < cube.Z1 then
            None
        else
            Some
                { X1 = max cube.X1 other.X1
                  X2 = min cube.X2 other.X2
                  Y1 = max cube.Y1 other.Y1
                  Y2 = min cube.Y2 other.Y2
                  Z1 = max cube.Z1 other.Z1
                  Z2 = min cube.Z2 other.Z2 }

let parse (text: string) =
    let on = text.StartsWith("on")

    let coordinates =
        text.Split(" ").[1].Split(",")
        |> Array.map (fun s ->
            let parts = s.Split("=").[1].Split("..")
            int parts.[0], int parts.[1])

    let cube =
        { X1 = fst coordinates.[0]
          X2 = snd coordinates.[0]
          Y1 = fst coordinates.[1]
          Y2 = snd coordinates.[1]
          Z1 = fst coordinates.[2]
          Z2 = snd coordinates.[2] }

    cube, on

let aggregate state ((currentCube, currentOn): Cube * bool) =
    state
    |> List.choose (fun (cube, on) ->
        currentCube.Intersect cube
        |> Option.map (fun i -> (i, not on)))
    |> List.append (
        if currentOn then
            [ currentCube, currentOn ]
        else
            []
    )
    |> List.append state

let getCount input (limit: int option) =
    input
    |> Array.map parse
    |> Array.filter (fun (cube, _) ->
        limit.IsNone
        || cube.Values
           |> Array.map abs
           |> Array.forall (fun v -> v <= limit.Value))
    |> Seq.fold aggregate []
    |> Seq.sumBy (fun (cube, on) -> cube.Volume * (if on then 1L else -1L))

let input = File.ReadAllLines "./input.txt"

let resultPart1 = getCount input (Some 50)
printfn "Result Part 1: %d" resultPart1

let resultPart2 = getCount input None
printfn "Result Part 2: %d" resultPart2

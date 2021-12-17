open System
open System.IO
open System.Text.RegularExpressions

let parse input =
    let regex = Regex("target area: x=(\d+)..(\d+), y=(-?\d+)..(-?\d+)")
    let matches = regex.Match(input)
    let x1, x2 = (int matches.Groups.[1].Value, int matches.Groups.[2].Value)
    let y1, y2 = (int matches.Groups.[3].Value, int matches.Groups.[4].Value)
    ((x1, x2), (y1, y2))

let getNewPosition (x, y) (vx, vy) = (x + vx, y + vy)

let getNewVelocity (x, y) =
    let xDrag =
        match x with
        | _ when x > 0 -> -1
        | _ when x < 0 -> 1
        | _ -> 0

    (x + xDrag, y - 1)

let isInTargetArea targetArea trajectory =
    let ((x1, x2), (y1, y2)) = targetArea

    let isPositionInTargetArea (x, y) =
        x >= x1 && x <= x2 && y >= y1 && y <= y2

    trajectory |> Seq.exists isPositionInTargetArea

type State =
    { Velocity: int * int
      Trajectory: (int * int) [] }

let getTrajectory targetArea initialVelocity =
    let ((x1, x2), (y1, y2)) = targetArea

    let initialState =
        { Velocity = initialVelocity
          Trajectory = [| (0, 0) |] }

    let getNextState state _ =
        let newPosition = getNewPosition (Array.last state.Trajectory) state.Velocity
        let newTrajectory = Array.append state.Trajectory [| newPosition |]

        { Velocity = getNewVelocity state.Velocity
          Trajectory = newTrajectory }

    let result =
        Seq.initInfinite id
        |> Seq.scan getNextState initialState
        |> Seq.takeWhile (fun x -> snd (Array.last x.Trajectory) >= y1)
        |> Seq.last

    if (result.Trajectory |> isInTargetArea targetArea) then
        Some result.Trajectory
    else
        None

let getAllTrajectories (targetArea: (int * int) * (int * int)) =
    let ((x1, x2), (y1, y2)) = targetArea

    Seq.allPairs [ 0..x2 ] [
        y1 .. Math.Abs(y1)
    ]
    |> Seq.choose (getTrajectory targetArea)

let getResult input =
    input
    |> parse
    |> getAllTrajectories
    |> Seq.map (Seq.map snd >> Seq.max)
    |> Seq.max

let getResult2 input =
    input |> parse |> getAllTrajectories |> Seq.length

let input = File.ReadAllText "./input.txt"

let resultPart1 = getResult input
printfn "Result Part 1: %d" resultPart1

let resultPart2 = getResult2 input
printfn "Result Part 2: %d" resultPart2

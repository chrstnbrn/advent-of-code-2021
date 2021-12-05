open System
open System.IO

type Coordinate = { X: int; Y: int }
type LineSegment = { P1: Coordinate; P2: Coordinate }

let parseCoordinate (text: string) =
    let parts = text.Split(",")
    { X = int parts.[0]; Y = int parts.[1] }

let parseLineSegment (text: string) =
    let coordinates =
        text.Split(" -> ") |> Array.map parseCoordinate

    { P1 = coordinates.[0]
      P2 = coordinates.[1] }

let parseLineSegments (input: string) =
    input.Split(Environment.NewLine)
    |> Array.map parseLineSegment

let isHorizontal lineSegment = lineSegment.P1.Y = lineSegment.P2.Y
let isVertical lineSegment = lineSegment.P1.X = lineSegment.P2.X

let isHorizontalOrVertical lineSegment =
    isHorizontal lineSegment || isVertical lineSegment

let getRange a b =
    let increment = if a < b then 1 else -1
    { a .. increment .. b }

let getXValues lineSegment =
    getRange lineSegment.P1.X lineSegment.P2.X

let getYValues lineSegment =
    getRange lineSegment.P1.Y lineSegment.P2.Y

let getCoordinatesOnLineSegment lineSegment =
    if isHorizontal lineSegment then
        lineSegment
        |> getXValues
        |> Seq.map (fun x -> { X = x; Y = lineSegment.P1.Y })
    else if isVertical lineSegment then
        lineSegment
        |> getYValues
        |> Seq.map (fun y -> { X = lineSegment.P1.X; Y = y })
    else
        Seq.zip (getXValues lineSegment) (getYValues lineSegment)
        |> Seq.map (fun (x, y) -> { X = x; Y = y })

let countOverlaps coordinates =
    coordinates
    |> Seq.countBy id
    |> Seq.filter (fun (_, count) -> count > 1)
    |> Seq.length

let getOverlaps input =
    input
    |> parseLineSegments
    |> Seq.filter isHorizontalOrVertical
    |> Seq.collect getCoordinatesOnLineSegment
    |> countOverlaps

let getOverlapsIncludingDiagonals input =
    input
    |> parseLineSegments
    |> Seq.collect getCoordinatesOnLineSegment
    |> countOverlaps

let input = File.ReadAllText "./input.txt"

let resultPart1 = getOverlaps input
printfn "Result Part 1: %d" resultPart1

let resultPart2 = getOverlapsIncludingDiagonals input
printfn "Result Part 2: %d" resultPart2

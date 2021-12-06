open System
open System.IO

type Coordinate =
    { X: int
      Y: int }
    static member parse(text: string) =
        let parts = text.Split(",")
        { X = int parts.[0]; Y = int parts.[1] }

type LineSegment =
    { P1: Coordinate
      P2: Coordinate }
    static member parse(text: string) =
        let coordinates = text.Split(" -> ") |> Array.map Coordinate.parse

        { P1 = coordinates.[0]
          P2 = coordinates.[1] }

    static member isHorizontal lineSegment = lineSegment.P1.Y = lineSegment.P2.Y
    static member isVertical lineSegment = lineSegment.P1.X = lineSegment.P2.X

    static member isHorizontalOrVertical lineSegment =
        LineSegment.isHorizontal lineSegment
        || LineSegment.isVertical lineSegment

    static member getCoordinates lineSegment =
        let getRange a b =
            let increment = if a < b then 1 else -1
            { a..increment..b }

        let xValues = getRange lineSegment.P1.X lineSegment.P2.X
        let yValues = getRange lineSegment.P1.Y lineSegment.P2.Y

        if LineSegment.isHorizontal lineSegment then
            xValues
            |> Seq.map (fun x -> { X = x; Y = lineSegment.P1.Y })
        else if LineSegment.isVertical lineSegment then
            yValues
            |> Seq.map (fun y -> { X = lineSegment.P1.X; Y = y })
        else
            Seq.zip xValues yValues
            |> Seq.map (fun (x, y) -> { X = x; Y = y })

let parseLineSegments (input: string) =
    input.Split(Environment.NewLine)
    |> Array.map LineSegment.parse

let countOverlaps coordinates =
    coordinates
    |> Seq.countBy id
    |> Seq.filter (fun (_, count) -> count > 1)
    |> Seq.length

let getOverlaps input =
    input
    |> parseLineSegments
    |> Seq.filter LineSegment.isHorizontalOrVertical
    |> Seq.collect LineSegment.getCoordinates
    |> countOverlaps

let getOverlapsIncludingDiagonals input =
    input
    |> parseLineSegments
    |> Seq.collect LineSegment.getCoordinates
    |> countOverlaps

let input = File.ReadAllText "./input.txt"

let resultPart1 = getOverlaps input
printfn "Result Part 1: %d" resultPart1

let resultPart2 = getOverlapsIncludingDiagonals input
printfn "Result Part 2: %d" resultPart2

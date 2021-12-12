open System.IO

type Connection =
    { From: string
      To: string }
    static member parse(text: string) =
        let parts = text.Split("-")
        { From = parts.[0]; To = parts.[1] }

let isUpperCase (text: string) = text.ToUpper() = text

let rec findPath connections (currentPath: string list) =
    let last = List.head currentPath

    if last = "end" then
        [| currentPath |]
    else
        let tos =
            connections
            |> Array.filter (fun c -> c.From = last)
            |> Array.map (fun c -> c.To)

        let froms =
            connections
            |> Array.filter (fun c -> c.To = last)
            |> Array.map (fun c -> c.From)

        Array.concat [| tos; froms |]
        |> Array.filter (fun c -> (isUpperCase c || not (List.contains c currentPath)))
        |> Array.collect (fun c -> findPath connections (c :: currentPath))


let rec findPath2 connections (currentPath: string list) =
    let last = List.head currentPath

    if last = "end" then
        [| currentPath |]
    else
        let tos =
            connections
            |> Array.filter (fun c -> c.From = last)
            |> Array.map (fun c -> c.To)

        let froms =
            connections
            |> Array.filter (fun c -> c.To = last)
            |> Array.map (fun c -> c.From)

        let hasVisitedSmallCaveTwice =
            currentPath
            |> Seq.filter (isUpperCase >> not)
            |> Seq.countBy id
            |> Seq.exists (fun (_, count) -> count > 1)

        Array.concat [| tos; froms |]
        |> Array.filter (fun c ->
            (isUpperCase c
             || not (List.contains c currentPath)
             || (c <> "start"
                 && c <> "end"
                 && not hasVisitedSmallCaveTwice)))
        |> Array.collect (fun c -> findPath2 connections (c :: currentPath))


let getNumberOfPaths input =
    let connections = input |> Array.map Connection.parse

    findPath connections [ "start" ] |> Seq.length

let getNumberOfPaths2 input =
    let connections = input |> Array.map Connection.parse

    findPath2 connections [ "start" ] |> Seq.length

let input = File.ReadAllLines "./input.txt"

let resultPart1 = getNumberOfPaths input
printfn "Result Part 1: %d" resultPart1

let resultPart2 = getNumberOfPaths2 input
printfn "Result Part 2: %d" resultPart2

open System
open System.IO

let getFuelCosts1 (a: int) (b: int) = Math.Abs(a - b)

let getFuelCosts2 (a: int) (b: int) =
    let n = Math.Abs(a - b)
    n * (n + 1) / 2

let getFuelToReachTarget positions getCosts target =
    positions |> Seq.sumBy (getCosts target)

let findMinFuel positions getCosts =
    let min = Array.min positions
    let max = Array.max positions

    [ min..max ]
    |> Seq.map (getFuelToReachTarget positions getCosts)
    |> Seq.min

let getMinFuel positions = findMinFuel positions getFuelCosts1
let getMinFuel2 positions = findMinFuel positions getFuelCosts2

let input = File.ReadAllText "./input.txt"
let positions = input.Split(",") |> Array.map int
let resultPart1 = getMinFuel positions
printfn "Result Part 1: %d" resultPart1

let resultPart2 = getMinFuel2 positions
printfn "Result Part 2: %d" resultPart2

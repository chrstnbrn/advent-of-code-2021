open System.IO

let countDepthIncreases =
    Seq.pairwise
    >> Seq.filter (fun (x, y) -> y > x)
    >> Seq.length

let countSlidingWindowDepthIncreases =
    Seq.windowed 3
    >> Seq.map Seq.sum
    >> countDepthIncreases

[<EntryPoint>]
let main argv =
    let measurements =
        File.ReadAllLines "./input.txt" |> Seq.map int

    let resultPart1 = countDepthIncreases measurements
    printfn "Part 1: %d measurements are larger than the previous measurement" resultPart1

    let resultPart2 =
        countSlidingWindowDepthIncreases measurements

    printfn "Part 2: %d measurements are larger than the previous measurement" resultPart2


    0 // return an integer exit code

open System.IO

type SignalPattern = char Set

let hasLength length pattern = Set.count pattern = length
let is1 pattern = hasLength 2 pattern
let is4 pattern = hasLength 4 pattern
let is7 pattern = hasLength 3 pattern
let is8 pattern = hasLength 7 pattern

let getDecodedPatterns (signals: SignalPattern array) =
    let one = signals |> Array.find is1
    let four = signals |> Array.find is4
    let seven = signals |> Array.find is7
    let eight = signals |> Array.find is8

    let nine =
        signals
        |> Array.find (fun x -> x.Count = 6 && Set.isSubset four x)

    let zero =
        signals
        |> Array.find (fun x -> x.Count = 6 && Set.isSubset one x && x <> nine)

    let six =
        signals
        |> Array.find (fun x -> x.Count = 6 && x <> nine && x <> zero)

    let three =
        signals
        |> Array.find (fun x -> x.Count = 5 && Set.isSubset one x)

    let five =
        signals
        |> Array.find (fun x -> x.Count = 5 && Set.isSubset x nine && x <> three)

    let two =
        signals
        |> Array.find (fun x -> x.Count = 5 && x <> three && x <> five)

    [ (zero, 0)
      (one, 1)
      (two, 2)
      (three, 3)
      (four, 4)
      (five, 5)
      (six, 6)
      (seven, 7)
      (eight, 8)
      (nine, 9) ]
    |> Map

type Entry =
    { SignalPatterns: SignalPattern array
      Output: SignalPattern array }
    static member parse(text: string) =
        let parseSignal = Seq.cast<char> >> Set

        let parts = text.Split(" | ")
        let signalPatterns = parts.[0].Split() |> Array.map parseSignal
        let output = parts.[1].Split() |> Array.map parseSignal

        { SignalPatterns = signalPatterns
          Output = output }

    static member decode(entry: Entry) =
        let decodedPatterns = getDecodedPatterns entry.SignalPatterns

        entry.Output
        |> Array.map (fun pattern -> string decodedPatterns.[pattern])
        |> String.concat ""
        |> int

let getResult input =
    input
    |> Array.map Entry.parse
    |> Array.collect (fun entry -> entry.Output)
    |> Seq.filter (fun x -> is1 x || is4 x || is7 x || is8 x)
    |> Seq.length

let getResult2 input =
    input
    |> Array.map Entry.parse
    |> Seq.sumBy Entry.decode

let input = File.ReadAllLines "./input.txt"
let resultPart1 = getResult input
printfn "Result Part 1: %d" resultPart1

let resultPart2 = getResult2 input
printfn "Result Part 2: %d" resultPart2

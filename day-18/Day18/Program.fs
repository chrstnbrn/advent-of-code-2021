open System
open System.IO
open System.Text.RegularExpressions

let replace (value: string) replacement i j =
    value.[.. i - 1] + replacement + value.[j + 1 ..]

let getExplodingIndex (number: string) =
    number
    |> Seq.scan
        (fun nesting ch ->
            match ch with
            | '[' -> nesting + 1
            | ']' -> nesting - 1
            | _ -> nesting)
        0
    |> Seq.tryFindIndex (fun n -> n > 4)
    |> Option.map (fun i -> i - 1)

let explodeLeft (number: string) left =
    number
    |> Seq.tryFindIndexBack Char.IsDigit
    |> Option.map (fun iEnd ->
        let iStart =
            (number.[0..iEnd]
             |> Seq.findIndexBack (Char.IsDigit >> not))
            + 1

        let previousNumber = int number.[iStart..iEnd]
        let newValue = previousNumber + left
        replace number (newValue.ToString()) iStart iEnd)
    |> Option.defaultValue number

let explodeRight (number: string) right =
    number
    |> Seq.tryFindIndex Char.IsDigit
    |> Option.map (fun iStart ->
        let iEnd =
            iStart
            + ((number.[iStart..]
                |> Seq.findIndex (Char.IsDigit >> not)))
            - 1

        let nextNumber = int number.[iStart..iEnd]
        let newNextNumber = nextNumber + right
        replace number (newNextNumber.ToString()) iStart iEnd)

    |> Option.defaultValue number

let explode (number: string) =
    number
    |> getExplodingIndex
    |> Option.map (fun i ->
        let endIndex = i + (number.[i..] |> Seq.findIndex ((=) ']'))

        let numbers =
            number.[(i + 1) .. (endIndex - 1)].Split(",")
            |> Array.map int

        let left = explodeLeft number.[0 .. i - 1] numbers.[0]
        let right = explodeRight number.[endIndex + 1 ..] numbers.[1]

        left + "0" + right

    )
    |> Option.defaultValue number

let split number =
    let m = Regex.Match(number, "\d{2,}")

    if m.Success then
        let (value, i, length) = (int m.Value, m.Index, m.Length)
        let left = value / 2
        let right = value - left

        sprintf "%s[%d,%d]%s" number.[.. i - 1] left right number.[i + length ..]
    else
        number

let applyWhileChanges f x =
    Seq.initInfinite id
    |> Seq.scan (fun n _ -> f n) x
    |> Seq.pairwise
    |> Seq.takeWhile (fun (a, b) -> a <> b)
    |> Seq.map snd
    |> Seq.tryLast
    |> Option.defaultValue x

let reduce number =
    number
    |> applyWhileChanges ((applyWhileChanges explode) >> split)

let add a b = "[" + a + "," + b + "]" |> reduce
let addMany x = x |> Seq.reduce add

let findSplitIndex (number: string) =
    number
    |> Seq.scan
        (fun nesting ch ->
            match ch with
            | '[' -> nesting + 1
            | ']' -> nesting - 1
            | _ -> nesting)
        0
    |> Seq.skip 1
    |> Seq.findIndex (fun n -> n = 0)
    |> ((+) 2)

let rec getMagnitude (x: string) =
    if x.[0] = '[' then
        let i = findSplitIndex x.[1 .. x.Length - 2]
        let (leftText, rightText) = (x.[1 .. i - 1], x.[i + 1 .. x.Length - 2])
        let left = getMagnitude leftText
        let right = getMagnitude rightText
        3 * left + 2 * right
    else
        int x

let getMagnitudeOfSum input = input |> addMany |> getMagnitude

let getLargestMagnitude (input: string array) =
    input
    |> Seq.allPairs input
    |> Seq.map (fun pair -> pair ||> add |> getMagnitude)
    |> Seq.max

let input = File.ReadAllLines "./input.txt"

let resultPart1 = getMagnitudeOfSum input
printfn "Result Part 1: %d" resultPart1

let resultPart2 = getLargestMagnitude input
printfn "Result Part 2: %d" resultPart2

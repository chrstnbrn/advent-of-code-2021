open System
open System.Collections
open System.IO

type DiagnosticReport = int [,]

let getDiagnosticReport (input: string array) =
    Array2D.init input.Length input[0].Length (fun i j -> input.[i].[j] |> string |> int)

let getRows diagnosticReport =
    let height = diagnosticReport |> Array2D.length1

    [ 0 .. height - 1 ]
    |> Seq.map (fun i -> diagnosticReport[i, *])

let getColumns diagnosticReport =
    let width = diagnosticReport |> Array2D.length2

    [ 0 .. width - 1 ]
    |> Seq.map (fun i -> diagnosticReport[*, i])

let bitsToNumber bits =
    bits
    |> Seq.map string
    |> String.concat ""
    |> (fun x -> Convert.ToInt32(x, 2))

let getGammaRateBit (values: int seq) =
    values |> Seq.countBy id |> Seq.maxBy snd |> fst

let getEpsilonRateBit (values: int seq) =
    values |> Seq.countBy id |> Seq.minBy snd |> fst

let getRate getBit diagnosticReport =
    diagnosticReport
    |> getColumns
    |> Seq.map getBit
    |> bitsToNumber

let getGammaRate diagnosticReport =
    diagnosticReport |> getRate getGammaRateBit

let getEpsilonRate diagnosticReport =
    diagnosticReport |> getRate getEpsilonRateBit

let getPowerConsumption (input: string array) =
    let diagnosticReport = getDiagnosticReport input
    let gammaRate = getGammaRate diagnosticReport
    let epsilonRate = getEpsilonRate diagnosticReport
    gammaRate * epsilonRate

let rec getRating bitCriteria (columnIndex: int) (diagnosticReport: DiagnosticReport) =
    let column = diagnosticReport[*, columnIndex]
    let bit = bitCriteria column

    let remainingValues =
        diagnosticReport
        |> getRows
        |> Seq.filter (fun x -> x[columnIndex] = bit)
        |> array2D

    if (Array2D.length1 remainingValues) = 1 then
        bitsToNumber remainingValues[0, *]
    else
        getRating bitCriteria (columnIndex + 1) remainingValues

let mostCommonBitCriteria bits =
    bits
    |> Seq.countBy id
    |> Seq.sortByDescending fst
    |> Seq.maxBy snd
    |> fst

let leastCommonBitCriteria bits =
    bits
    |> Seq.countBy id
    |> Seq.sortBy fst
    |> Seq.minBy snd
    |> fst

let getOxygenGeneratorRating diagnosticReport =
    getRating mostCommonBitCriteria 0 diagnosticReport

let rec getCo2ScrubberRating diagnosticReport =
    getRating leastCommonBitCriteria 0 diagnosticReport

let getLifeSupportRating (input: string array) =
    let diagnosticReport = getDiagnosticReport input
    let oxgenGeneratorRating = getOxygenGeneratorRating diagnosticReport
    let scrubberRating = getCo2ScrubberRating diagnosticReport
    oxgenGeneratorRating * scrubberRating

let input = File.ReadAllLines "./input.txt"

let resultPart1 = getPowerConsumption input

printfn "Result Part 1: %d" resultPart1

let resultPart2 = getLifeSupportRating input
printfn "Result Part 2: %d" resultPart2

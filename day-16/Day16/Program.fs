open System
open System.IO

type Packet =
    | Literal of LiteralPacket
    | Operator of OperatorPacket

and LiteralPacket = { Version: int; Value: int64 }

and OperatorPacket =
    { Version: int
      Operator: int
      SubPackets: Packet [] }

let toBinary hex =
    hex
    |> Seq.cast<char>
    |> Seq.map (fun ch ->
        let number = Convert.ToInt32(string ch, 16)
        Convert.ToString(number, 2).PadLeft(4, '0'))
    |> String.concat ""
    |> Seq.cast<char>
    |> Seq.map (string >> int)
    |> Seq.toArray

let toDecimal bits =
    bits
    |> Seq.map string
    |> String.concat ""
    |> (fun x -> Convert.ToInt32(x, 2))

let toDecimal64 bits =
    bits
    |> Seq.map string
    |> String.concat ""
    |> (fun x -> Convert.ToInt64(x, 2))

let rec parsePacket (bits: int array) : (Packet * int array) =
    let version = bits.[0..2] |> toDecimal
    let typeId = bits.[3..5] |> toDecimal
    let body = bits.[6..]

    match (typeId, body.[0]) with
    | (4, _) -> parseLiteralPacket body version
    | (_, 0) -> parseOperatorPacketMode0 body version typeId
    | (_, 1) -> parseOperatorPacketMode1 body version typeId
    | _ -> failwithf "invalid type length id %d" body.[0]

and parseLiteralPacket body version =
    let chunks = body |> Array.chunkBySize 5
    let index = chunks |> Seq.findIndex (Array.head >> ((=) 0))

    let valueBits = chunks.[0..index] |> Array.collect (Array.skip 1)
    let remainingBits = chunks.[index + 1 ..] |> Array.collect id

    let value = toDecimal64 valueBits
    let packet = Literal { Version = version; Value = value }

    (packet, remainingBits)

and parseOperatorPacketMode0 body version typeId =
    let subPacketsLength = body.[1..15] |> toDecimal
    let otherBits = body.[16..]

    let (subPackets, remainingBits) =
        Seq.initInfinite id
        |> Seq.scan parseSubPackets ([||], otherBits)
        |> Seq.find (fun (_, r) -> r.Length = (otherBits.Length - subPacketsLength))

    let operatorPacket =
        { Version = version
          Operator = typeId
          SubPackets = subPackets }

    (Operator operatorPacket, remainingBits)

and parseOperatorPacketMode1 body version typeId =
    let numberOfSubPackets = body.[1..11] |> toDecimal
    let otherBits = body.[12..]

    let (subPackets, remainingsBits) =
        [ 1..numberOfSubPackets ]
        |> Seq.fold parseSubPackets ([||], otherBits)

    let operatorPacket =
        { Version = version
          Operator = typeId
          SubPackets = subPackets }

    (Operator operatorPacket, remainingsBits)

and parseSubPackets (currentPackets, currentRemainingBits) _ =
    let (packet, newRemainingBits) = parsePacket currentRemainingBits
    let newPackets = Array.append currentPackets [| packet |]
    (newPackets, newRemainingBits)

let aggregate x typeId =
    match typeId with
    | 0 -> x |> Array.sum
    | 1 -> x |> Array.reduce (*)
    | 2 -> x |> Array.min
    | 3 -> x |> Array.max
    | 5 -> if x.[0] > x.[1] then 1L else 0L
    | 6 -> if x.[0] < x.[1] then 1L else 0L
    | 7 -> if x.[0] = x.[1] then 1L else 0L
    | _ -> failwithf "invalid type id %d" typeId

let rec getVersionSumRec packet =
    match packet with
    | Literal x -> x.Version
    | Operator o ->
        o.Version
        + (o.SubPackets |> Seq.sumBy getVersionSumRec)

let getVersionSum input =
    input
    |> toBinary
    |> parsePacket
    |> fst
    |> getVersionSumRec

let rec getValueRec packet =
    match packet with
    | Literal l -> l.Value
    | Operator o ->
        let values = o.SubPackets |> Array.map (getValueRec)
        aggregate values o.Operator

let getValue input =
    input
    |> toBinary
    |> parsePacket
    |> fst
    |> getValueRec

let input = File.ReadAllText "./input.txt"

let resultPart1 = getVersionSum input
printfn "Result Part 1: %d" resultPart1

let resultPart2 = getValue input
printfn "Result Part 2: %d" resultPart2

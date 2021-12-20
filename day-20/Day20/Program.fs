open System
open System.IO

let toInt pixels =
    let bits =
        pixels
        |> Seq.map (fun x -> if x = '.' then "0" else "1")
        |> String.concat ""

    Convert.ToInt32(bits, 2)

let enhance (algorithm: string) (image: char [,]) i =
    let background = if i % 2 = 0 then algorithm.[0] else '.'

    let b1, b2 = Array2D.base1 image, Array2D.base2 image
    let l1, l2 = Array2D.length1 image, Array2D.length2 image

    let biggerImage = Array2D.createBased (b1 - 2) (b2 - 2) (l1 + 4) (l2 + 4) background
    Array2D.blit image b1 b2 biggerImage b1 b1 l1 l2

    Array2D.initBased (b1 - 1) (b2 - 1) (l1 + 2) (l2 + 2) (fun i j ->
        let index =
            biggerImage.[i - 1 .. i + 1, j - 1 .. j + 1]
            |> Seq.cast<char>
            |> toInt

        algorithm.[index])

let getLitPixels (input: string []) count =
    let algorithm = input.[0]
    let inputImage = array2D input.[2..]

    [ 1..count ]
    |> Seq.fold (enhance algorithm) inputImage
    |> Seq.cast<char>
    |> Seq.filter ((=) '#')
    |> Seq.length

let input = File.ReadAllLines "./input.txt"

let resultPart1 = getLitPixels input 2
printfn "Result Part 1: %d" resultPart1

let resultPart2 = getLitPixels input 50
printfn "Result Part 2: %d" resultPart2

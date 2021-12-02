open System.IO

type Position = {X: int; Y: int; Aim: int}
type Command = { direction: Direction; value: int }
and Direction = | Up | Down | Forward

let initialPosition() = {X = 0; Y = 0; Aim = 0}

let parseCommand (commandText: string) =
    let parts = commandText.Split [|' '|]
    let direction = match parts[0] with
                        | "up" -> Up
                        | "down" -> Down
                        | "forward" -> Forward
                        | _ as c -> failwithf "invalid command %s" c
    let value = int parts[1]
    { direction = direction;  value = value }

let parseCommands commandTexts = commandTexts |> Seq.map parseCommand

let moveByCommand (position: Position) (command: Command) =
    match command.direction with
        | Up -> {position with Y = position.Y - command.value}
        | Down -> {position with Y = position.Y + command.value}
        | Forward -> {position with X = position.X + command.value}

let moveByCommand2 (position: Position) (command: Command) =
    match command.direction with
        | Up -> {position with Aim = position.Aim - command.value}
        | Down -> {position with Aim = position.Aim + command.value}
        | Forward -> {position with X = position.X + command.value; Y = position.Y + position.Aim * command.value}

let move commandTexts = commandTexts |> parseCommands |> Seq.fold moveByCommand (initialPosition ())
let move2 commandTexts = commandTexts |> parseCommands |> Seq.fold moveByCommand2 (initialPosition ())

let commandTexts = File.ReadAllLines "./input.txt"

let position = move commandTexts
let resultPart1 = position.X * position.Y
printfn "Part 1: %d" resultPart1


let position2 = move2 commandTexts
let resultPart2 = position2.X * position2.Y

printfn "Part 2: %d" resultPart2

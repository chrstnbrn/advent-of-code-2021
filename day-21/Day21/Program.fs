open System.IO
open System.Collections.Generic

let parseInput (input: string array) =
    let positions =
        input
        |> Array.map (fun s -> s.Split(": ").[1] |> int)

    (positions.[0], positions.[1])

type PlayerState = { Position: int; Score: int }

type GameState =
    { Round: int
      Player1State: PlayerState
      Player2State: PlayerState }

let getNewPlayerState state role =
    let sum = Array.sum role

    let newPosition = (state.Position + sum - 1) % 10 + 1

    let newScore = state.Score + newPosition

    { Position = newPosition
      Score = newScore }

let getNewPlayerState2 state sum =
    let newPosition = (state.Position + sum - 1) % 10 + 1

    let newScore = state.Score + newPosition

    { Position = newPosition
      Score = newScore }

let playRound state (die: int []) =
    let player1Role = die.[0..2]
    let player2Role = die.[3..5]

    { Round = state.Round + 1
      Player1State = getNewPlayerState state.Player1State player1Role
      Player2State = getNewPlayerState state.Player2State player2Role }

let getDieRoles () =
    seq {
        for i in [ 1..3 ] do
            for j in [ 1..3 ] do
                for k in [ 1..3 ] do
                    yield [| i; j; k |]
    }
    |> Seq.toList

let playRoundDiradic state =
    let player1States =
        getDieRoles ()
        |> List.map (fun role -> getNewPlayerState state.Player1State role)

    player1States
    |> List.collect (fun player1State ->
        let player2States =
            getDieRoles ()
            |> List.map (fun role -> getNewPlayerState state.Player2State role)

        player2States
        |> List.map (fun player2State ->
            { Round = state.Round + 1
              Player1State = player1State
              Player2State = player2State }))

let rec countPlayerWins game (cache: Dictionary<GameState, int64 * int64>) =
    if game.Player2State.Score >= 21 then
        (0L, 1L)
    else if cache.ContainsKey(game) then
        cache.[game]
    else
        let result =
            getDieRoles ()
            |> List.map (Array.sum)
            |> List.countBy id
            |> Seq.fold
                (fun (count1, count2) (sum, count) ->
                    let playerState = getNewPlayerState2 game.Player1State sum

                    let newGame =
                        { game with
                            Player1State = game.Player2State
                            Player2State = playerState }

                    let p2, p1 = countPlayerWins newGame cache
                    count1 + p1 * (int64 count), count2 + p2 * (int64 count))
                (0L, 0L)

        cache.Add(game, result)
        result

let getResult input =
    let (positionPlayer1, positionPlayer2) = parseInput input

    let gameState =
        { Round = 0
          Player1State =
            { Position = positionPlayer1
              Score = 0 }
          Player2State =
            { Position = positionPlayer2
              Score = 0 } }

    let (secondToLastState, lastState) =
        Seq.initInfinite id
        |> Seq.collect (fun _ -> [ 1..100 ])
        |> Seq.chunkBySize 6
        |> Seq.scan (fun state i -> playRound state i) gameState
        |> Seq.pairwise
        |> Seq.find (fun (f, s) ->
            s.Player1State.Score >= 1000
            || s.Player2State.Score >= 1000)

    let losingPlayerState =
        if lastState.Player1State.Score >= 1000 then
            secondToLastState.Player2State
        else
            secondToLastState.Player1State

    let diceRoles = lastState.Round * 6 - 3
    diceRoles * losingPlayerState.Score

let getResult2 input =
    let (positionPlayer1, positionPlayer2) = parseInput input

    let gameState =
        { Round = 0
          Player1State =
            { Position = positionPlayer1
              Score = 0 }
          Player2State =
            { Position = positionPlayer2
              Score = 0 } }

    let p1, p2 = countPlayerWins gameState (Dictionary<GameState, int64 * int64>())
    max p1 p2

let input = File.ReadAllLines "./input.txt"

let resultPart1 = getResult input
printfn "Result Part 1: %d" resultPart1

let resultPart2 = getResult2 input
printfn "Result Part 2: %d" resultPart2

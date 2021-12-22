open System.Text.RegularExpressions

let parseInput (input : string) : int * int =
    match input.Trim().Split "\n" with
    | [| line1; line2 |] ->
        let match1 = Regex.Match(line1, "Player 1 starting position: (\\d+)")
        let match2 = Regex.Match(line2, "Player 2 starting position: (\\d+)")
        int match1.Groups[1].Value, int match2.Groups[1].Value
    | _ -> failwith "invalid input"

type Player =
    { Position : int
    ; Score : int
    }

let toPlayer (position : int) : Player =
    { Position = position
    ; Score = 0
    }

type Game =
    { DieRolls : int
    ; Player1 : Player
    ; Player2 : Player
    ; Turn : int
    }

let toGame ((position1, position2) : int * int) : Game =
    { DieRolls = 0
    ; Player1 = toPlayer position1
    ; Player2 = toPlayer position2
    ; Turn = 0
    }

let takeTurn (game : Game) : Game =
    let r1 = 1 + (game.DieRolls % 100)
    let r2 = 1 + ((game.DieRolls + 1) % 100)
    let r3 = 1 + ((game.DieRolls + 2) % 100)
    let f = r1 + r2 + r3
    let isPlayer1 = game.Turn % 2 = 0
    let player =
        if isPlayer1 then
            let position = 1 + ((game.Player1.Position + f - 1) % 10)
            { Position = position
            ; Score = game.Player1.Score + position
            }
        else
            let position = 1 + ((game.Player2.Position + f - 1) % 10)
            { Position = position
            ; Score = game.Player2.Score + position
            }
    { DieRolls = game.DieRolls + 3
    ; Player1 = if isPlayer1 then player else game.Player1
    ; Player2 = if isPlayer1 then game.Player2 else player
    ; Turn = game.Turn + 1
    }

let hasEnded (game : Game) : bool =
    game.Player1.Score >= 1000 || game.Player2.Score >= 1000

let rec solve (game : Game) : int =
    if hasEnded game then
        min game.Player1.Score game.Player2.Score * game.DieRolls
    else
        solve (takeTurn game)

System.IO.File.ReadAllText "21/input.txt"
|> parseInput
|> toGame
|> solve
|> printfn "%A"

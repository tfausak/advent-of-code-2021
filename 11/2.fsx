let charToInt (c : char) : int =
    int c - int '0'

type Point = { X : int; Y : int }

let toMap (vss : 'v seq seq) : Map<Point, 'v> =
    vss
    |> Seq.mapi (fun y vs -> Seq.mapi (fun x v -> { X = x; Y = y }, v) vs)
    |> Seq.concat
    |> Map.ofSeq

let getNeighbors (p : Point) : Point Set =
    Set.ofList
        [ { X = p.X - 1; Y = p.Y - 1 } // NW
        ; { X = p.X; Y = p.Y - 1 } // N
        ; { X = p.X + 1; Y = p.Y - 1 } // NE
        ; { X = p.X + 1; Y = p.Y } // E
        ; { X = p.X + 1; Y = p.Y + 1 } // SE
        ; { X = p.X; Y = p.Y + 1 } // S
        ; { X = p.X - 1; Y = p.Y + 1 } // SW
        ; { X = p.X - 1; Y = p.Y } // W
        ]

let rec flash (m : Map<Point, int * bool>) : Map<Point, int * bool> =
    let picker point (energy, flash) =
        if not flash && energy > 9 then
            Some (point, energy)
        else
            None
    let folder map point =
        match Map.tryFind point map with
        | None -> map
        | Some (energy, flash) -> Map.add point (energy + 1, flash) map
    match Map.tryPick picker m with
    | None -> m
    | Some (point, energy) ->
        point
        |> getNeighbors
        |> Seq.fold folder (Map.add point (energy, true) m)
        |> flash

let step (m : Map<Point, int>) : Map<Point, int> =
    m
    |> Map.map (fun _ v -> v + 1, false)
    |> flash
    |> Map.map (fun _ v -> if snd v then 0 else fst v)

let rec solve (i : int) (m : Map<Point, int>) : int =
    if Map.fold (fun a _ v -> a && v = 0) true m then
        i
    else
        solve (i + 1) (step m)

System.IO.File.ReadLines "11/input.txt"
|> Seq.map (Seq.map charToInt)
|> toMap
|> solve 0
|> printfn "%A"

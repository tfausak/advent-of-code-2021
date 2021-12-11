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
    match Map.tryPick picker m with
    | None -> m
    | Some (point, energy) ->
        point
        |> getNeighbors
        |> Seq.fold
            (fun n p ->
                match Map.tryFind p n with
                | None -> n
                | Some (e, f) -> Map.add p (e + 1, f) n)
            (Map.add point (energy, true) m)
        |> flash

let step (m : Map<Point, int>) : Map<Point, int> =
    m
    |> Map.map (fun _ v -> v + 1, false)
    |> flash
    |> Map.map (fun _ v -> if snd v then 0 else fst v)

let rec iterate (i : int) (f : 'a -> 'a) (x : 'a) : 'a =
    if i > 0 then
        iterate (i - 1) f (f x)
    else
        x

System.IO.File.ReadLines "11/input.txt"
|> Seq.map (Seq.map charToInt)
|> toMap
|> fun m -> m, 0
|> iterate 100 (fun (m, fs) ->
    let n = step m
    n, fs + Map.count (Map.filter (fun _ v -> v = 0) n))
|> printfn "%A"

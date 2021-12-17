// https://www.redblobgames.com/pathfinding/a-star/introduction.html

type Risk =
    | Risk of int

type Point =
    { X : int
    ; Y : int
    }

type Cave =
    Map<Point, Risk>

type Priority =
    | Priority of int

let toRisk (c : char) : Risk option =
    match c with
    | '0' -> Some (Risk 0)
    | '1' -> Some (Risk 1)
    | '2' -> Some (Risk 2)
    | '3' -> Some (Risk 3)
    | '4' -> Some (Risk 4)
    | '5' -> Some (Risk 5)
    | '6' -> Some (Risk 6)
    | '7' -> Some (Risk 7)
    | '8' -> Some (Risk 8)
    | '9' -> Some (Risk 9)
    | _ -> None

let toCave (rss : 'r seq seq) : Map<Point, 'r> =
    rss
    |> Seq.mapi (fun y -> Seq.mapi (fun x r -> { X = x; Y = y }, r))
    |> Seq.concat
    |> Map.ofSeq

let getNeighbors (p : Point) : Point Set =
    Set.ofSeq
        [ { p with X = p.X - 1 } // left
        ; { p with Y = p.Y - 1 } // up
        ; { p with Y = p.Y + 1 } // down
        ; { p with X = p.X + 1 } // right
        ]

let getMinimum (rs : 'r Set) : ('r * 'r Set) option =
    if Set.isEmpty rs then
        None
    else
        let r = Set.minElement rs
        Some (r, Set.remove r rs)

let addRisk (Risk x) (Risk y) : Risk =
    Risk (x + y)

let riskToPriority (Risk x) : Priority =
    Priority x

let rec dijkstraWith (cave : Cave) (frontier : (Priority * Point) Set) (cameFrom : Map<Point, Point Option>) (riskSoFar : Map<Point, Risk>) : Map<Point, Risk> =
    match getMinimum frontier with
    | None -> riskSoFar
    | Some ((_priority, current), newFrontier) ->
        // printfn "- %d, %d: %A" current.X current.Y _priority
        let risk =
            match Map.tryFind current riskSoFar with
            | None -> Risk 0
            | Some r -> r
        let ps =
            getNeighbors current
            |> Seq.choose (fun next ->
                match Map.tryFind next cave with
                | None -> None
                | Some r ->
                    let newRisk = addRisk risk r
                    match Map.tryFind next riskSoFar with
                    | None -> Some (next, newRisk)
                    | Some oldRisk when newRisk < oldRisk -> Some (next, newRisk)
                    | _ -> None)
        dijkstraWith
            cave
            (Seq.fold (fun s (p, r) -> Set.add (riskToPriority r, p) s) newFrontier ps)
            (Seq.fold (fun m (p, _) -> Map.add p (Some current) m) cameFrom ps)
            (Seq.fold (fun m (p, r) -> Map.add p r m) riskSoFar ps)

let dijkstra (c : Cave) : Map<Point, Risk> =
    let p = { X = 0; Y = 0 }
    dijkstraWith
        c
        (Set.ofSeq [Priority 0, p])
        (Map.ofSeq [p, None])
        (Map.ofSeq [p, Risk 0])

System.IO.File.ReadLines "15/input.txt"
|> Seq.map (fun s -> s.ToCharArray() |> Seq.map (toRisk >> Option.get))
|> toCave
|> dijkstra
|> Map.find { X = 99; Y = 99 }
|> printfn "%A"

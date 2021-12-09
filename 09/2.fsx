type Point = { X : int; Y : int }

let toMap (vss : 'v seq seq) : Map<Point, 'v> =
    vss
    |> Seq.mapi (fun y vs -> Seq.mapi (fun x v -> ( { X = x; Y = y }, v)) vs)
    |> Seq.concat
    |> Map.ofSeq

let getNeighbors (p : Point) : Point Set =
    Set.ofList
        [ { p with X = p.X - 1 }
        ; { p with X = p.X + 1 }
        ; { p with Y = p.Y - 1 }
        ; { p with Y = p.Y + 1 }
        ]

let setPop (s : 'a Set) : ('a * 'a Set) option =
    if Set.isEmpty s then
        None
    else
        let e = Set.minElement s
        Some (e, Set.remove e s)

let rec getAllNeighbors (known : Point Set) (visited : Point Set) (queue : Point Set) (p : Point) : Point Set =
    let newVisited = Set.add p visited
    let neighbors =
        p
        |> getNeighbors
        |> Set.filter (fun x -> Set.contains x known && not (Set.contains x newVisited))
    match setPop (Set.union queue neighbors) with
    | None -> newVisited
    | Some (h, newQueue) -> getAllNeighbors known newVisited newQueue h

let mapKeys (m : Map<'k, 'v>) : 'k Set =
    m
    |> Map.keys
    |> Set.ofSeq

let rec solve (b : int) (m : Map<Point, int option>) : Map<Point, int> =
    let getUnvisited point basin =
        if Option.isNone basin then
            Some point
        else
            None
    match Map.tryPick getUnvisited m with
    | None -> Map.map (fun _ basin -> Option.get basin) m
    | Some point ->
        getAllNeighbors (mapKeys m) Set.empty Set.empty point
        |> Seq.fold (fun n p -> Map.add p (Some b) n) m
        |> solve (b + 1)

let tally (ks : 'k seq) : Map<'k, int> =
    let f o =
        match o with
        | None -> Some 1
        | Some n -> Some (n + 1)
    Seq.fold (fun m k -> Map.change k f m) Map.empty ks

System.IO.File.ReadLines "09/input.txt"
    |> Seq.map (fun x -> x.ToCharArray() |> Array.toSeq)
    |> toMap
    |> Map.filter (fun _ height -> not (height = '9'))
    |> Map.map (fun _ _ -> None)
    |> solve 0
    |> Map.values
    |> tally
    |> Map.values
    |> Seq.sortDescending
    |> Seq.take 3
    |> Seq.fold (fun a e -> a * e) 1
    |> printfn "%A"

type V3<'a> = 'a * 'a * 'a

let allOrientations : int V3 V3 list =
    [ (( 1,  0,  0), ( 0,  1,  0), ( 0,  0,  1))
    ; (( 1,  0,  0), ( 0,  0, -1), ( 0,  1,  0))
    ; (( 1,  0,  0), ( 0, -1,  0), ( 0,  0, -1))
    ; (( 1,  0,  0), ( 0,  0,  1), ( 0, -1,  0))
    ; (( 0, -1,  0), ( 1,  0,  0), ( 0,  0,  1))
    ; (( 0,  0,  1), ( 1,  0,  0), ( 0,  1,  0))
    ; (( 0,  1,  0), ( 1,  0,  0), ( 0,  0, -1))
    ; (( 0,  0, -1), ( 1,  0,  0), ( 0, -1,  0))
    ; ((-1,  0,  0), ( 0, -1,  0), ( 0,  0,  1))
    ; ((-1,  0,  0), ( 0,  0, -1), ( 0, -1,  0))
    ; ((-1,  0,  0), ( 0,  1,  0), ( 0,  0, -1))
    ; ((-1,  0,  0), ( 0,  0,  1), ( 0,  1,  0))
    ; (( 0,  1,  0), (-1,  0,  0), ( 0,  0,  1))
    ; (( 0,  0,  1), (-1,  0,  0), ( 0, -1,  0))
    ; (( 0, -1,  0), (-1,  0,  0), ( 0,  0, -1))
    ; (( 0,  0, -1), (-1,  0,  0), ( 0,  1,  0))
    ; (( 0,  0, -1), ( 0,  1,  0), ( 1,  0,  0))
    ; (( 0,  1,  0), ( 0,  0,  1), ( 1,  0,  0))
    ; (( 0,  0,  1), ( 0, -1,  0), ( 1,  0,  0))
    ; (( 0, -1,  0), ( 0,  0, -1), ( 1,  0,  0))
    ; (( 0,  0, -1), ( 0, -1,  0), (-1,  0,  0))
    ; (( 0, -1,  0), ( 0,  0,  1), (-1,  0,  0))
    ; (( 0,  0,  1), ( 0,  1,  0), (-1,  0,  0))
    ; (( 0,  1,  0), ( 0,  0, -1), (-1,  0,  0))
    ]

let reorient ((x, y, z) : int V3) (((x1, y1, z1), (x2, y2, z2), (x3, y3, z3)) : int V3 V3) : int V3 =
    ( x * x1 + y * y1 + z * z1
    , x * x2 + y * y2 + z * z2
    , x * x3 + y * y3 + z * z3
    )

let negate ((x, y, z) : int V3) : int V3 =
    (-x, -y, -z)

let add ((x1, y1, z1) : int V3) ((x2, y2, z2) : int V3) : int V3 =
    (x1 + x2, y1 + y2, z1 + z2)

let allTranslations (vs : int V3 seq) (ws : int V3 seq) : int V3 Set =
    vs
    |> Seq.collect (fun v -> Seq.map (fun w -> add v (negate w)) ws)
    |> Set.ofSeq

let findOverlap (beacons : int V3 Set) (original : int V3 Set) : (int V3 * int V3 Set) option =
    printfn "  findOverlap %d %d" (Set.count beacons) (Set.count original)
    allOrientations
    |> Seq.tryPick (fun orientation ->
        let candidate = Set.map (fun point -> reorient point orientation) original
        let scanners = allTranslations beacons candidate
        scanners
        |> Seq.tryPick (fun scanner ->
            let newBeacons = Set.map (fun point -> add point scanner) candidate
            if Set.count (Set.intersect beacons newBeacons) >= 12 then
                Some (scanner, newBeacons)
            else
                None))

let rec solveWith (scanners : int V3 Set) (beacons : int V3 Set) (inbox : int V3 Set list) (outbox : int V3 Set list) : int V3 Set * int V3 Set =
    printfn "solveWith %d" (Set.count beacons)
    match inbox with
    | [] ->
        match outbox with
        | [] -> scanners, beacons
        | _ ->
            printfn "solveWith: looping"
            solveWith scanners beacons outbox []
    | first :: rest ->
        match findOverlap beacons first with
        | None -> solveWith scanners beacons rest (first :: outbox)
        | Some (scanner, newBeacons) -> solveWith (Set.add scanner scanners) (Set.union beacons newBeacons) rest outbox

let solve (scanners : int V3 Set seq) : int V3 Set * int V3 Set =
    match List.ofSeq scanners with
    | [] -> Set.empty, Set.empty
    | first :: rest -> solveWith (Set.singleton (0, 0, 0)) first rest []

let distance ((x1, y1, z1) : int V3) ((x2, y2, z2) : int V3) : int =
    abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)

System.IO.File.ReadAllText "19/input.txt"
|> fun input -> input.Trim().Split "\n\n"
|> Seq.map (fun chunk ->
    chunk.Split "\n"
    |> Seq.skip 1
    |> Seq.map (fun line ->
        line.Split ","
        |> Seq.map int
        |> fun point ->
            match Seq.toList point with
            | [ x; y; z ] -> V3 (x, y, z)
            | _ -> failwithf "invalid point: %A" point)
    |> Set.ofSeq)
|> solve
|> fun (scanners, _) -> Seq.allPairs scanners scanners
|> Seq.map (fun (x, y) -> distance x y)
|> Seq.max
|> printfn "%A"

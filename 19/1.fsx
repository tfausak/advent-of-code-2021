// each scanner thinks it's at (0, 0, 0)
// each scanner reports beacon locations relative to itself
// scanners can only "see" 1,000 units along each axis
// each scanner can be in one of 24 different orientations
// combine scanners that have at least 12 beacons overlapping
// once done, count the number of beacons

// hard question: are two groups of points overlapping up to a translation,
// given that each group may only be a partial subset of the other?

// easier question: find all possible translations, then count the number of
// overlaps for each translation. take the translation with the greatest number
// of overlaps. and for this problem, the number of overlaps needs to be at
// least 12

type V3<'a> = 'a * 'a * 'a

// https://www.euclideanspace.com/maths/algebra/matrix/transforms/examples/index.htm
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

// https://mathinsight.org/matrix_vector_multiplication
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

let findOverlap (beacons : int V3 Set) (original : int V3 Set) : int V3 Set option =
    allOrientations
    |> Seq.tryPick (fun orientation ->
        let candidate = Set.map (fun point -> reorient point orientation) original
        let translations = allTranslations beacons candidate
        translations
        |> Seq.tryPick (fun translation ->
            let newBeacons = Set.map (fun point -> add point translation) candidate
            if Set.count (Set.intersect beacons newBeacons) >= 12 then
                Some newBeacons
            else
                None))

let rec solveWith (beacons : int V3 Set) (inbox : int V3 Set list) (outbox : int V3 Set list) : int V3 Set =
    match inbox with
    | [] ->
        match outbox with
        | [] -> beacons
        | _ -> solveWith beacons outbox []
    | first :: rest ->
        match findOverlap beacons first with
        | None -> solveWith beacons rest (first :: outbox)
        | Some newBeacons -> solveWith (Set.union beacons newBeacons) rest outbox

let solve (scanners : int V3 Set seq) : int V3 Set =
    match List.ofSeq scanners with
    | [] -> Set.empty
    | first :: rest -> solveWith first rest []

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
|> Set.count
|> printfn "%A"

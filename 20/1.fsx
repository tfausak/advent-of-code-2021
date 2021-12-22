type Point = int * int

let getWindow ((x, y) : Point) : Point seq =
    Seq.ofList
        [ x - 1, y - 1
        ; x, y - 1
        ; x + 1, y - 1
        ; x - 1, y
        ; x, y
        ; x + 1, y
        ; x - 1, y + 1
        ; x, y + 1
        ; x + 1, y + 1
        ]

let findWithDefault (v : 'v) (k : 'k) (m : Map<'k, 'v>) : 'v =
    m
    |> Map.tryFind k
    |> Option.defaultValue v

let fromBinary (bs : bool seq) : int =
    Seq.fold (fun n b -> n * 2 + if b then 1 else 0) 0 bs

let isLit (fallback : bool) (algorithm : Map<int, bool>) (image : Map<Point, bool>) (point : Point) : bool =
    point
    |> getWindow
    |> Seq.map (fun p -> findWithDefault fallback p image)
    |> fromBinary
    |> fun index -> Map.find index algorithm

let enhance (algorithm : Map<int, bool>) ((fallback, image) : bool * Map<Point, bool>) : bool * Map<Point, bool> =
    let xs = Seq.map fst (Map.keys image)
    let (minX, maxX) = Seq.min xs, Seq.max xs
    let ys = Seq.map snd (Map.keys image)
    let (minY, maxY) = Seq.min ys, Seq.max ys
    seq { minX - 1 .. maxX + 1 }
    |> Seq.collect (fun x ->
        seq { minY - 1 .. maxY + 1 }
        |> Seq.map (fun y ->
            let point = x, y
            point, isLit fallback algorithm image point))
    |> Map.ofSeq
    |> fun m -> not fallback, m

let rec iterate (f : 'a -> 'a) (i : int) (x : 'a) : 'a =
    if i > 0 then iterate f (i - 1) (f x) else x

System.IO.File.ReadAllText "20/input.txt"
|> fun input ->
    match input.Trim().Split "\n\n" with
    | [| algorithm; image |] ->
        ( algorithm
            |> Seq.mapi (fun index pixel -> index, pixel = '#')
            |> Map.ofSeq
        , image.Split "\n"
            |> Seq.mapi (fun y -> Seq.mapi (fun x pixel -> (x, y), pixel = '#'))
            |> Seq.concat
            |> Map.ofSeq
        )
    | _ -> failwith "invalid input"
|> fun (algorithm, image) -> iterate (enhance algorithm) 2 (false, image)
|> snd
|> Map.values
|> Seq.filter id
|> Seq.length
|> printfn "%A"

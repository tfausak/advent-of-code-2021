let seqToMapWith (f : 'v -> 'v -> 'v) (xs : seq<'k * 'v>) : Map<'k, 'v> =
    let f x o =
        match o with
        | None -> x
        | Some y -> f x y
    xs
    |> Seq.fold (fun m (k, v) -> m.Change(k, Some << f v)) Map.empty

let toFrequencyMap (ks : 'k seq) : Map<'k, bigint> =
    let f (o : bigint option) : bigint option =
        o
        |> Option.defaultValue (bigint 0)
        |> fun x -> x + bigint 1
        |> Some
    Seq.fold (fun m k -> m.Change(k, f)) Map.empty ks

let step (m : Map<int, bigint>) : Map<int, bigint> =
    m
    |> Map.toSeq
    |> Seq.collect (fun (k, v) ->
        if k = 0 then
            [6, v; 8, v]
        else
            [k - 1, v])
    |> seqToMapWith (fun x y -> x + y)

let rec iterate (i : int) (f : 'a -> 'a) (x : 'a) : 'a =
    if i > 0 then
        iterate (i - 1) f (f x)
    else
        x

System.IO.File.ReadAllText("06/input.txt")
    |> fun x -> x.Trim()
    |> fun x -> x.Split(",")
    |> Seq.map int
    |> toFrequencyMap
    |> iterate 256 step
    |> Map.values
    |> Seq.sum
    |> printfn "%A"

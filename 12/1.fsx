let groupBy (f : 'v -> 'k) (vs : 'v seq) : Map<'k, 'v Set> =
    let changer x o =
        match o with
        | None -> Some (Set.singleton x)
        | Some xs -> Some (Set.add x xs)
    Seq.fold (fun m v -> Map.change (f v) (changer v) m) Map.empty vs

let rec findPathsWith (m : Map<string, string Set>) (paths : string seq seq) (path : string list) (current : string) : string seq seq =
    let newPath = current :: path
    if current = "end" then
        Seq.append (Seq.singleton (Seq.rev newPath)) paths
    else
        m
        |> Map.tryFind current
        |> Option.defaultValue Set.empty
        |> Set.filter (fun cave ->
            String.forall System.Char.IsUpper cave
            || not (List.contains cave path))
        |> Seq.collect (findPathsWith m paths newPath)

let findPaths (m : Map<string, string Set>) : string seq seq =
    findPathsWith m [] [] "start"

System.IO.File.ReadLines "12/input.txt"
|> Seq.choose (fun line ->
    match line.Split "-" with
    | [| from; into |] -> Some (from, into)
    | _ -> None)
|> Seq.collect (fun (from, into) -> [ from, into; into, from ])
|> groupBy fst
|> Map.map (fun _ -> Set.map snd)
|> findPaths
|> Seq.length
|> printfn "%A"

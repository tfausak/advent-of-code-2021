let groupBy (f : 'v -> 'k) (vs : 'v seq) : Map<'k, 'v Set> =
    let changer x o =
        match o with
        | None -> Some (Set.singleton x)
        | Some xs -> Some (Set.add x xs)
    Seq.fold (fun m v -> Map.change (f v) (changer v) m) Map.empty vs

let rec findPathsWith (m : Map<string, string Set>) (paths : string seq seq) (path : string list) (twice : bool) (current : string) : string seq seq =
    let newPath = current :: path
    if current = "end" then
        newPath
        |> Seq.rev
        |> Seq.singleton
        |> Seq.append paths
    else
        m
        |> Map.tryFind current
        |> Option.defaultValue Set.empty
        |> Seq.collect (fun target ->
            if String.forall System.Char.IsUpper target then
                findPathsWith m paths newPath twice target
            elif target = "start" then
                Seq.empty
            elif List.contains target path then
                if twice then
                    Seq.empty
                else
                    findPathsWith m paths newPath true target
            else
                findPathsWith m paths newPath twice target)

let findPaths (m : Map<string, string Set>) : string seq seq =
    findPathsWith m [] [] false "start"

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

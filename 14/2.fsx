let insertWith (f : 'v -> 'v -> 'v) (k : 'k) (v : 'v) (m : Map<'k, 'v>) : Map<'k, 'v> =
    Map.change k (Option.fold f v >> Some) m

let tally (ks : 'k seq) : Map<'k, int64> =
    Seq.fold (fun m k -> insertWith (+) k 1 m) Map.empty ks

let rec iterate (f : 'a -> 'a) (i : int) (a : 'a) : 'a =
    if i < 1 then a else iterate f (i - 1) (f a)

let step (rules : Map<char * char, char>) (polymer : Map<char * char, int64>) : Map<char * char, int64> =
    let folder m pair count =
        match Map.tryFind pair rules with
        | None -> insertWith (+) pair count m
        | Some element ->
            m
            |> insertWith (+) (fst pair, element) count
            |> insertWith (+) (element, snd pair) count
    polymer
    |> Map.fold folder Map.empty
    |> Map.filter (fun _ count -> count > 0)

System.IO.File.ReadAllText "14/input.txt"
|> fun input ->
    match input.Trim().Split "\n\n" with
    | [| template; rules |] ->
        ( template
            |> Seq.pairwise
            |> tally
        , rules.Split "\n"
            |> Seq.map (fun rule ->
                match rule.Split " -> " with
                | [| pair; element |] when pair.Length = 2 && element.Length = 1 ->
                    (pair[0], pair[1]), element[0]
                | _ -> failwith "bad rule")
            |> Map.ofSeq
        )
    | _ -> failwith "bad input"
|> fun (template, rules) -> iterate (step rules) 40 template
|> Map.fold
    (fun m (x, y) v ->
        m
        |> insertWith (+) x v)
    Map.empty
|> Map.values
|> fun xs -> Seq.max xs - Seq.min xs - 1L
|> printfn "%d"

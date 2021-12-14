let rec iterate (f : 'a -> 'a) (i : int) (a : 'a) : 'a =
    if i < 1 then a else iterate f (i - 1) (f a)

let step (m : Map<char * char, char>) (cs : char[]) : char[] =
    seq { 1 .. cs.Length - 1 }
    |> Seq.collect (fun j ->
        let i = j - 1
        let l = cs[i]
        let r = cs[j]
        match Map.tryFind (l, r) m with
        | None -> Seq.singleton r
        | Some c -> Seq.ofList [c; r])
    |> Seq.append (Seq.singleton cs[0])
    |> Array.ofSeq

let tally (ks : 'k seq) : Map<'k, int> =
    let f o =
        match o with
        | None -> Some 1
        | Some n -> Some (n + 1)
    Seq.fold (fun m k -> Map.change k f m) Map.empty ks

System.IO.File.ReadAllText "14/input.txt"
|> fun line ->
    match line.Split "\n\n" with
    | [| template; rules |] ->
        ( template.ToCharArray()
        , rules.Split "\n"
            |> Seq.filter (fun rule -> rule.Length > 0)
            |> Seq.map (fun rule ->
                match rule.Split " -> " with
                | [| i; o |] when i.Length = 2 && o.Length = 1 ->
                    (i[0], i[1]), o[0]
                | _ -> failwith "bad rule")
            |> Map.ofSeq
        )
    | _ -> failwith "bad input"
|> fun (template, rules) -> iterate (step rules) 10 template
|> tally
|> Map.values
|> fun xs -> Seq.max xs - Seq.min xs
|> printfn "%A"

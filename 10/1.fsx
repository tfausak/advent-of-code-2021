let rec f2 (stack : char list) (s : char list) : char option =
    match s with
    | [] -> None
    | h :: t ->
        match h with
        | '(' -> f2 (h :: stack) t
        | '[' -> f2 (h :: stack) t
        | '{' -> f2 (h :: stack) t
        | '<' -> f2 (h :: stack) t
        | ')' ->
            match stack with
            | '(' :: rest -> f2 rest t
            | _ -> Some h
        | ']' ->
            match stack with
            | '[' :: rest -> f2 rest t
            | _ -> Some h
        | '}' ->
            match stack with
            | '{' :: rest -> f2 rest t
            | _ -> Some h
        | '>' ->
            match stack with
            | '<' :: rest -> f2 rest t
            | _ -> Some h
        | _ -> failwith "unexpected character"

let f1 (s : string) : char option =
    f2 [] (List.ofSeq s)

let tally (ks : 'k seq) : Map<'k, int> =
    let f o =
        match o with
        | None -> Some 1
        | Some n -> Some (n + 1)
    Seq.fold (fun m k -> Map.change k f m) Map.empty ks

let f3 (c : char) : int =
    match c with
    | ')' -> 3
    | ']' -> 57
    | '}' -> 1197
    | '>' -> 25137
    | _ -> failwith "unknown char"

System.IO.File.ReadLines "10/input.txt"
    |> Seq.choose f1
    |> tally
    |> Map.fold (fun a k v -> a + f3 k * v) 0
    |> printfn "%A"

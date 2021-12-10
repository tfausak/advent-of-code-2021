let rec f2 (stack : char list) (s : char list) : (char list) option =
    match s with
    | [] -> Some stack
    | h :: t ->
        match h with
        | '(' -> f2 (h :: stack) t
        | '[' -> f2 (h :: stack) t
        | '{' -> f2 (h :: stack) t
        | '<' -> f2 (h :: stack) t
        | ')' ->
            match stack with
            | '(' :: rest -> f2 rest t
            | _ -> None
        | ']' ->
            match stack with
            | '[' :: rest -> f2 rest t
            | _ -> None
        | '}' ->
            match stack with
            | '{' :: rest -> f2 rest t
            | _ -> None
        | '>' ->
            match stack with
            | '<' :: rest -> f2 rest t
            | _ -> None
        | _ -> failwith "unexpected character"

let f1 (s : string) : (char list) option =
    f2 [] (List.ofSeq s)

let f3 (c : char) : bigint =
    match c with
    | ')' -> bigint 1
    | ']' -> bigint 2
    | '}' -> bigint 3
    | '>' -> bigint 4
    | _ -> failwith "unknown char"

let f4 (c : char) : char =
    match c with
    | '(' -> ')'
    | '[' -> ']'
    | '{' -> '}'
    | '<' -> '>'
    | _ -> failwith "unknown bracket"

let score (cs : char seq) : bigint =
    Seq.fold (fun a e -> bigint 5 * a + f3 e) (bigint 0) cs

let getMiddle (xs : 'a seq) : 'a =
    xs
    |> Array.ofSeq
    |> fun ys -> ys[(Array.length ys - 1) / 2]

System.IO.File.ReadLines "10/input.txt"
    |> Seq.choose f1
    |> Seq.map (Seq.map f4)
    |> Seq.map score
    |> Seq.sort
    |> getMiddle
    |> printfn "%A"

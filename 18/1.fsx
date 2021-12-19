let uncons (s : 'a seq) : ('a * 'a seq) option =
    if Seq.isEmpty s then
        None
    else
        Some (Seq.head s, Seq.tail s)

let fold1 (f : 'a -> 'a -> 'a) (s : 'a seq) : 'a =
    match uncons s with
    | Some (h, t) -> Seq.fold f h t
    | _ -> failwithf "fold1: empty sequence: %A" s

let span (f : 'a -> bool) (s : 'a seq) : 'a seq * 'a seq =
    Seq.takeWhile f s, Seq.skipWhile f s

let isDigit (c : char) : bool =
    '0' <= c && c <= '9'

let toInt (s : char seq) : int option =
    try
        s |> System.String.Concat |> int |> Some
    with :? System.FormatException ->
        None

let parseInt (s1 : char seq) : (int * char seq) option =
    let (cs, s2) = span isDigit s1
    match toInt cs with
    | Some i -> Some (i, s2)
    | _ -> None

module Tree =
    type Tree<'a> =
        | Node of 'a
        | Pair of 'a Tree * 'a Tree

    let rec parse (f : char seq -> ('a * char seq) option) (s1 : char seq) : ('a Tree * char seq) option =
        match f s1 with
        | Some (x, s2) -> Some (Node x, s2)
        | _ ->
            match uncons s1 with
            | Some (l, s2) when l = '[' ->
                match parse f s2 with
                | Some (x, s3) ->
                    match uncons s3 with
                    | Some (c, s4) when c = ',' ->
                        match parse f s4 with
                        | Some (y, s5) ->
                            match uncons s5 with
                            | Some (r, s6) when r = ']' -> Some (Pair (x, y), s6)
                            | _ -> None
                        | _ -> None
                    | _ -> None
                | _ -> None
            | _ -> None

module Number =
    type Number =
        int Tree.Tree

    let parse (s : char seq) : (Number * char seq) option =
        Tree.parse parseInt s

    let rec addLeft (i : int) (n : Number) : Number =
        match n with
        | Tree.Node x -> Tree.Node (x + i)
        | Tree.Pair (l, r) -> Tree.Pair (addLeft i l, r)

    let rec addRight (i : int) (n : Number) : Number =
        match n with
        | Tree.Node x -> Tree.Node (x + i)
        | Tree.Pair (l, r) -> Tree.Pair (l, addRight i r)

    let rec explodeWith (i : int) (n : Number) : (int * Number * int) option =
        if i >= 4 then
            match n with
            | Tree.Pair (Tree.Node l, Tree.Node r) -> Some (l, Tree.Node 0, r)
            | _ -> None
        else
            match n with
            | Tree.Pair (l1, r1) ->
                match explodeWith (i + 1) l1 with
                | Some (ll, l2, rr) -> Some (ll, Tree.Pair (l2, addLeft rr r1), 0)
                | None ->
                    match explodeWith (i + 1) r1 with
                    | Some (ll, r2, rr) -> Some (0, Tree.Pair (addRight ll l1, r2), rr)
                    | None -> None
            | _ -> None

    let explode (n1 : Number) : Number option =
        match explodeWith 0 n1 with
        | Some (_, n2, _) -> Some n2
        | _ -> None

    let rec split (n : Number) : Number option =
        match n with
        | Tree.Node x ->
            if x >= 10 then
                Some (Tree.Pair (Tree.Node (x / 2), Tree.Node ((x + 1) / 2)))
            else
                None
        | Tree.Pair (l1, r1) ->
            match split l1 with
            | Some l2 -> Some (Tree.Pair (l2, r1))
            | None ->
                match split r1 with
                | Some r2 -> Some (Tree.Pair (l1, r2))
                | None -> None

    let rec reduce (n1 : Number) : Number =
        match explode n1 with
        | Some n2 -> reduce n2
        | None ->
            match split n1 with
            | Some n2 -> reduce n2
            | None -> n1

    let add (l : Number) (r : Number) : Number =
        reduce (Tree.Pair (l, r))

    let rec magnitude (n : Number) : int =
        match n with
        | Tree.Node x -> x
        | Tree.Pair (l, r) -> 3 * magnitude l + 2 * magnitude r

System.IO.File.ReadLines "18/input.txt"
|> Seq.map (fun line ->
    match Number.parse line with
    | Some (number, s) when Seq.isEmpty s -> number
    | _ -> failwithf "invalid input: %A" line)
|> fold1 Number.add
|> Number.magnitude
|> printfn "%A"

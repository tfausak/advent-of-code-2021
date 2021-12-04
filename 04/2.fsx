type Board = (int * bool) seq seq

let isWinner (board : Board) : bool =
    let h = Seq.exists (Seq.forall snd) board
    let v = Seq.exists (Seq.forall snd) (Seq.transpose board)
    h || v

let rec solve1 (numbers : int list) (boards : Board seq) =
    match numbers with
    | [] -> failwith "no more numbers"
    | number :: newNumbers ->
        let newBoards = Seq.map (Seq.map (Seq.map (fun (n, m) -> (n, m || n = number)))) boards
        match Seq.tryFind isWinner newBoards with
        | None -> solve1 newNumbers newBoards
        | Some winner ->
            winner
            |> Seq.concat
            |> Seq.choose (fun (n, m) -> if m then None else Some n)
            |> Seq.sum
            |> fun x -> x * number

let rec solve2 (numbers : int list) (boards : Board seq) =
    match numbers with
    | [] -> failwith "no more numbers"
    | number :: newNumbers ->
        let newBoards =
            boards
            |> Seq.map (Seq.map (Seq.map (fun (n, m) -> (n, m || n = number))))
            |> Seq.choose (fun b -> if isWinner b then None else Some b)
        match Seq.tryExactlyOne newBoards with
        | None -> solve2 newNumbers newBoards
        | Some _ -> solve1 newNumbers newBoards

System.IO.File.ReadAllText("04/input.txt")
    |> fun x ->
        match x.Split "\n\n" |> Array.toList with
        | [] -> failwith "bad input"
        | first :: rest ->
            ( first.Split ","
                |> Seq.map int
            , rest
                |> Seq.map (fun x -> x.Split "\n")
                |> Seq.map (Seq.filter (fun x -> not (x = "")))
                |> Seq.map (Seq.map (fun x -> x.Split " "))
                |> Seq.map (Seq.map (Seq.filter (fun x -> not (x = ""))))
                |> Seq.map (Seq.map (Seq.map (fun x -> int x, false)))
            )
    |> fun (numbers, boards) -> solve2 (Seq.toList numbers) boards
    |> printfn "%A"

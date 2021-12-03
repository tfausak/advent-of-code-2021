let binaryToDecimal (xs : seq<int>) =
    Seq.fold (fun a e -> 2 * a + e) 0 xs

System.IO.File.ReadLines "03/input.txt"
    |> Seq.transpose
    |> Seq.map (Seq.choose (fun x ->
        match x with
        | '0' -> Some -1
        | '1' -> Some 1
        | _ -> None))
    |> Seq.map Seq.sum
    |> Seq.map (fun x -> if x > 0 then 1, 0 else 0, 1)
    |> Seq.toList
    |> List.unzip
    |> fun (x, y) -> binaryToDecimal x, binaryToDecimal y
    |> fun (x, y) -> x * y
    |> printfn "%A"

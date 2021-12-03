System.IO.File.ReadLines "01/input.txt"
    |> Seq.map int
    |> Seq.windowed 3
    |> Seq.map Seq.sum
    |> Seq.pairwise
    |> Seq.where (fun (x, y) -> y > x)
    |> Seq.length
    |> printfn "%A"

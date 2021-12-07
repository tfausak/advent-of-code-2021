let solveWith (xs : int seq) (y : int) : int =
    xs
    |> Seq.map (fun x -> abs (x - y))
    |> Seq.sum

let solve (xs : int seq) : int =
    xs
    |> Seq.map (fun x -> solveWith xs x)
    |> Seq.min

System.IO.File.ReadAllText("07/input.txt")
    |> fun x -> x.Trim()
    |> fun x -> x.Split(",")
    |> Seq.map int
    |> solve
    |> printfn "%A"

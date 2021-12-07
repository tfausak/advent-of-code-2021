let rec fuelWith (acc : int) (x : int) : int =
    if x <= 0 then
        acc
    else
        fuelWith (acc + x) (x - 1)

let fuel (x : int) : int =
    fuelWith 0 x

let distance (x : int) (y : int) : int =
    x - y
    |> abs
    |> fuel

let solveWith (xs : int seq) (x : int) : int =
    xs
    |> Seq.map (distance x)
    |> Seq.sum

let solve (xs : int seq) : int =
    seq { Seq.min xs .. Seq.max xs }
    |> Seq.map (solveWith xs)
    |> Seq.min

System.IO.File.ReadAllText("07/input.txt")
    |> fun x -> x.Trim()
    |> fun x -> x.Split(",")
    |> Seq.map int
    |> solve
    |> printfn "%A"

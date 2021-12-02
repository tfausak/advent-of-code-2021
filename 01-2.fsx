// https://adventofcode.com/2021/day/1
System.IO.File.ReadLines "input/01.txt"
    |> Seq.map int
    |> Seq.windowed 3
    |> Seq.map Seq.sum
    |> Seq.pairwise
    |> Seq.where (fun (x, y) -> y > x)
    |> Seq.length
    |> printfn "%A"

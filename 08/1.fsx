// 0 abc efg // 6
// 1   c  f  // 2
// 2 a cde g // 5
// 3 a cd fg // 5
// 4  bcd f  // 4
// 5 ab d fg // 5
// 6 ab defg // 6
// 7 a c  f  // 3
// 8 abcdefg // 7
// 9 abcd fg // 6

System.IO.File.ReadLines "08/input.txt"
    |> Seq.map (fun x -> x.Split " | ")
    |> Seq.choose (fun x ->
        match x with
        | [| _; y |] -> Some y
        | _ -> None)
    |> Seq.collect (fun x -> x.Split " ")
    |> Seq.filter (fun x ->
        x.Length = 2
        || x.Length = 3
        || x.Length = 4
        || x.Length = 7)
    |> Seq.length
    |> printfn "%A"

System.IO.File.ReadLines "input/1.txt"
    |> Seq.map int
    |> Seq.fold
        (fun (count, optionLast) this ->
            let newCount =
                match optionLast with
                | Some last when this > last -> count + 1
                | _ -> count
            (newCount, Some this))
        (0, None)
    |> fst
    |> printfn "%A"

let getCount xs =
    xs
    |> Seq.map (fun x -> if x then 1 else -1)
    |> Seq.sum

let isNonNegative x = x >= 0

let getMostCommon xs =
    xs
    |> getCount
    |> isNonNegative

let getLeastCommon xs =
    xs
    |> getMostCommon
    |> not

let fromDigits b ds =
    Seq.fold (fun a d -> b * a + d) 0 ds

let rec solveWith (criteria : bool seq -> bool) (bits : bool list) (numbers : bool seq seq) =
    if Seq.isEmpty numbers then bits else
        match Seq.tryExactlyOne numbers with
        | Some x -> Seq.fold (fun a e -> e :: a) bits x
        | None ->
            let bit = numbers |> Seq.map Seq.head |> criteria
            numbers
                |> Seq.choose (fun x -> if Seq.head x = bit then Some (Seq.tail x) else None)
                |> solveWith criteria (bit :: bits)

let solve criteria numbers =
    solveWith criteria [] numbers
    |> Seq.map (fun x -> if x then 1 else 0)
    |> Seq.rev
    |> fromDigits 2

let diagnostic =
    System.IO.File.ReadLines "03/input.txt"
    |> Seq.map (Seq.map (fun x -> x = '1'))

solve getMostCommon diagnostic * solve getLeastCommon diagnostic
    |> printfn "%A"

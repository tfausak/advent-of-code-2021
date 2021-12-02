// https://adventofcode.com/2021/day/2
type Command =
    | Down of int
    | Forward of int
    | Up of int

let parseCommand (x : string) =
    match x.Split " " with
    | [| "down"; n |] -> Some (Down (int n))
    | [| "forward"; n |] -> Some (Forward (int n))
    | [| "up"; n |] -> Some (Up (int n))
    | _ -> None

System.IO.File.ReadLines "input/02.txt"
    |> Seq.choose parseCommand
    |> Seq.fold
        (fun (h, d, a) c ->
            match c with
            | Down x -> (h, d, a + x)
            | Forward x -> (h + x, d + (x * a), a)
            | Up x -> (h, d, a - x))
        (0, 0, 0)
    |> (fun (h, d, _) -> h * d)
    |> printfn "%A"

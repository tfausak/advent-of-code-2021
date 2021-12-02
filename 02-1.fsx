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
        (fun (h, d) c ->
            match c with
            | Down x -> (h, d + x)
            | Forward x -> (h + x, d)
            | Up x -> (h, d - x))
        (0, 0)
    |> (fun (h, d) -> h * d)
    |> printfn "%A"

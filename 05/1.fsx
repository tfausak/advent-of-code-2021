let stringToInt (s : string) : int option =
    try
        Some (int s)
    with :? System.FormatException ->
        None

type Point = { X : int; Y : int }

let stringToPoint (s : string) : Point option =
    match s.Split(",") with
    | [| xs; ys |] ->
        match stringToInt xs, stringToInt ys with
        | Some x, Some y -> Some { X = x; Y = y }
        | _ -> None
    | _ -> None

type Line = { Start : Point; End : Point }

let stringToLine (s : string) : Line option =
    match s.Split(" -> ") with
    | [| ps; qs |] ->
        match stringToPoint ps, stringToPoint qs with
        | Some p, Some q -> Some { Start = p; End = q }
        | _ -> None
    | _ -> None

let isHorizontal (l : Line) : bool =
    l.Start.Y = l.End.Y

let isVertical (l : Line) : bool =
    l.Start.X = l.End.X

let isCardinal (l : Line) : bool =
    isHorizontal l || isVertical l

let range (a : int) (b : int) : int seq =
    if a > b then
        seq { a .. -1 .. b }
    else
        seq { a .. b }

let lineToPoints (l : Line) : Point seq =
    if isHorizontal l then
        seq { for x in range l.Start.X l.End.X -> { X = x; Y = l.Start.Y } }
    elif isVertical l then
        seq { for y in range l.Start.Y l.End.Y -> { X = l.Start.X; Y = y } }
    else
        failwith "neither horizontal nor vertical"

let toFrequencyMap (xs : 'a seq) : Map<'a, int> =
    let f x =
        match x with
        | Some y -> Some (y + 1)
        | None -> Some 1
    Seq.fold (fun a x -> a.Change(x, f)) Map.empty xs

System.IO.File.ReadLines "05/input.txt"
    |> Seq.choose stringToLine
    |> Seq.filter isCardinal
    |> Seq.map lineToPoints
    |> Seq.concat
    |> toFrequencyMap
    |> Map.filter (fun _ v -> v >= 2)
    |> Map.count
    |> printfn "%A"

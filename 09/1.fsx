let charToInt (c : char) : int =
    int c - int '0'

let toMap (css : string seq) : Map<int * int, int> =
    css
    |> Seq.mapi (fun y cs -> Seq.mapi (fun x c -> ((x, y), charToInt c)) cs)
    |> Seq.concat
    |> Map.ofSeq

let getNeighborPoints ((x, y) : int * int) : (int * int) seq =
    Seq.ofList
        [ x - 1, y
        ; x + 1, y
        ; x, y - 1
        ; x, y + 1
        ]

let getNeighbors (m : Map<int * int, int>) (i : int * int) : int seq =
    getNeighborPoints i
    |> Seq.choose (fun k -> Map.tryFind k m)

System.IO.File.ReadLines "09/input.txt"
    |> toMap
    |> fun m ->
        Map.filter
            (fun k v ->
                getNeighbors m k
                |> Seq.exists (fun n -> n <= v)
                |> not)
            m
    |> Map.values
    |> Seq.map (fun x -> x + 1)
    |> Seq.sum
    |> printfn "%A"

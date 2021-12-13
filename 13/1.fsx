type Coordinate =
    { X : int
    ; Y : int
    }

type Instruction =
    | X of int
    | Y of int

let folder (cs : Coordinate Set) (i : Instruction) : Coordinate Set =
    let f c =
        match i with
        | X x ->
            let d = c.X - x
            if d > 0 then
                { c with X = c.X - 2 * d }
            else
                c
        | Y y ->
            let d = c.Y - y
            if d > 0 then
                { c with Y = c.Y - 2 * d }
            else
                c
    Set.map f cs

System.IO.File.ReadAllText "13/input.txt"
|> fun input ->
    match input.Split "\n\n" with
    | [| dots; instructions |] ->
        ( dots.Split "\n"
            |> Seq.map (fun line ->
                match line.Split "," with
                | [| x; y |] -> { X = int x; Y = int y }
                | _ -> failwith "invalid coordinate")
            |> Set.ofSeq
        , instructions.Trim().Split "\n"
            |> Seq.map (fun line ->
                match line.Split "=" with
                | [| "fold along x"; x |] -> X (int x)
                | [| "fold along y"; y |] -> Y (int y)
                | _ -> failwith "invalid instruction")
            |> Seq.take 1
        )
    | _ -> failwith "invalid input"
|> fun (coordinates, instructions) ->
    Seq.fold folder coordinates instructions
|> Set.count
|> printfn "%A"

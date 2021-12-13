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

let render (cs : Coordinate Set) : string =
    let minX = cs |> Seq.map (fun c -> c.X) |> Seq.min
    let maxX = cs |> Seq.map (fun c -> c.X) |> Seq.max
    let minY = cs |> Seq.map (fun c -> c.Y) |> Seq.min
    let maxY = cs |> Seq.map (fun c -> c.Y) |> Seq.max
    seq { minY .. maxY }
    |> Seq.map (fun y ->
        seq { minX .. maxX }
        |> Seq.map (fun x ->
            let c = { X = x; Y = y }
            if Set.contains c cs then
                "#"
            else
                ".")
        |> String.concat "")
    |> String.concat "\n"

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
        )
    | _ -> failwith "invalid input"
|> fun (coordinates, instructions) ->
    Seq.fold folder coordinates instructions
|> render
|> printfn "%s"

(*
###...##..###..#..#..##..###..#..#.#...
#..#.#..#.#..#.#..#.#..#.#..#.#.#..#...
#..#.#....#..#.####.#..#.#..#.##...#...
###..#....###..#..#.####.###..#.#..#...
#....#..#.#....#..#.#..#.#.#..#.#..#...
#.....##..#....#..#.#..#.#..#.#..#.####
*)

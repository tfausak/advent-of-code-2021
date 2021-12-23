open System.Text.RegularExpressions

type Point = { X : int; Y : int; Z : int }

System.IO.File.ReadLines "22/input.txt"
|> Seq.map (fun line ->
    let pattern = @"^(on|off) x=(.+)\.\.(.+),y=(.+)\.\.(.+),z=(.+)\.\.(.+)$"
    let groups = Regex.Match(line, pattern).Groups
    let isOn = groups[1].Value = "on"
    let point0 =
        { X = int groups[2].Value
        ; Y = int groups[4].Value
        ; Z = int groups[6].Value
        }
    let point1 =
        { X = int groups[3].Value
        ; Y = int groups[5].Value
        ; Z = int groups[7].Value
        }
    isOn, point0, point1)
|> Seq.filter (fun (_, p, q) ->
    let f x = -50 <= x && x <= 50
    let g x = f x.X && f x.Y && f x.Z
    g p && g q)
|> Seq.fold
    (fun points (b, p, q) ->
        let newPoints =
            seq { p.X .. q.X }
            |> Seq.collect (fun x ->
                seq { p.Y .. q.Y }
                |> Seq.collect (fun y ->
                    seq { p.Z .. q.Z }
                    |> Seq.map (fun z -> { X = x; Y = y; Z = z })))
            |> Set.ofSeq
        if b then
            Set.union points newPoints
        else
            Set.difference points newPoints)
    Set.empty
|> Set.count
|> printfn "%A"

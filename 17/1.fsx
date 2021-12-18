open System.Text.RegularExpressions

type Position =
    { X : int
    ; Y : int
    }

type Velocity =
    { DX : int
    ; DY : int
    }

type Probe =
    { Position : Position
    ; Velocity : Velocity
    }

type Area =
    { MinX : int
    ; MaxX : int
    ; MinY : int
    ; MaxY : int
    }

let parseInput (s : string) : Area =
    let m = Regex.Match(s, "target area: x=(.+)[.]{2}(.+), y=(.+)[.]{2}(.+)")
    { MinX = int m.Groups[1].Value
    ; MaxX = int m.Groups[2].Value
    ; MinY = int m.Groups[3].Value
    ; MaxY = int m.Groups[4].Value
    }

let step (probe : Probe) : Probe =
    { Position =
        { X = probe.Position.X + probe.Velocity.DX
        ; Y = probe.Position.Y + probe.Velocity.DY
        }
    ; Velocity =
        { DX =
            match probe.Velocity.DX with
            | 0 -> 0
            | dx -> if dx > 0 then dx - 1 else dx + 1
        ; DY = probe.Velocity.DY - 1
        }
    }

let inArea (area : Area) (position : Position) : bool =
    position.X >= area.MinX
        && position.X <= area.MaxX
        && position.Y >= area.MinY
        && position.Y <= area.MaxY

let rec simulate (pass : bool) (positions : Position list) (probe : Probe) (area : Area) : bool * Position list =
    if probe.Position.X > area.MaxX || probe.Position.Y < area.MinY then
        pass, positions
    else
        simulate (pass || inArea area probe.Position) (probe.Position :: positions) (step probe) area

let area =
    System.IO.File.ReadAllText "17/input.txt"
    |> parseInput

let mutable peakY = 0
for dx = 0 to area.MaxX do
    if Seq.sum (seq { 1 .. dx }) >= area.MinX then
        for dy = 0 to max (abs area.MinY) (abs area.MaxY) do
            let probe = { Position = { X = 0; Y = 0 }; Velocity = { DX = dx; DY = dy } }
            let (pass, positions) = simulate false [] probe area
            if pass then
                let maxY = positions |> Seq.map (fun p -> p.Y) |> Seq.max
                peakY <- max peakY maxY
                printfn "- %A, %A: %A" dx dy maxY
printfn "%A" peakY

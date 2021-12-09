// For each digit, which segments are illuminated?
// 0: abc efg
// 1:   c  f
// 2: a cde g
// 3: a cd fg
// 4:  bcd f
// 5: ab d fg
// 6: ab defg
// 7: a c  f
// 8: abcdefg
// 9: abcd fg

// For each segment, how many digits does it appear in?
// a: 8
// b: 6
// c: 8
// d: 7
// e: 4
// f: 9
// g: 7

// For each digit, what is the sum of its segments?
// 0: 42
// 1: 17
// 2: 34
// 3: 39
// 4: 30
// 5: 37
// 6: 41
// 7: 25
// 8: 49
// 9: 45

let digitMap : Map<int, int> =
    Map.ofSeq
        [ 42, 0
        ; 17, 1
        ; 34, 2
        ; 39, 3
        ; 30, 4
        ; 37, 5
        ; 41, 6
        ; 25, 7
        ; 49, 8
        ; 45, 9
        ]

let tally (ks : 'k seq) : Map<'k, int> =
    let f o =
        match o with
        | None -> Some 1
        | Some n -> Some (n + 1)
    Seq.fold (fun m k -> Map.change k f m) Map.empty ks

System.IO.File.ReadLines "08/input.txt"
    |> Seq.choose (fun line ->
        match line.Split " | " with
        | [| unique; output |] -> Some (unique.Split " ", output.Split " ")
        | _ -> None)
    |> Seq.map (fun (unique, output) ->
        let counts =
            unique
            |> Seq.concat
            |> tally
        output
        |> Seq.map (fun digit ->
            digit
            |> Seq.map (fun segment -> counts[segment])
            |> Seq.sum
            |> fun count -> digitMap[count])
        |> Seq.fold (fun n d -> n * 10 + d) 0)
    |> Seq.sum
    |> printfn "%A"

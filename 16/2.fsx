type Value<'packet> =
    | Literal of int64
    | Operator of 'packet list

type Packet =
    { version : int64
    ; typeId : int64
    ; value : Packet Value
    }

let hexadecimalToBinary (c : char) : string =
    match c with
    | '0' -> "0000"
    | '1' -> "0001"
    | '2' -> "0010"
    | '3' -> "0011"
    | '4' -> "0100"
    | '5' -> "0101"
    | '6' -> "0110"
    | '7' -> "0111"
    | '8' -> "1000"
    | '9' -> "1001"
    | 'A' -> "1010"
    | 'B' -> "1011"
    | 'C' -> "1100"
    | 'D' -> "1101"
    | 'E' -> "1110"
    | 'F' -> "1111"
    | _ -> failwithf "invalid hexadecimal: %A" c

let binaryToDecimal (cs : char seq) : int64 =
    Seq.fold
        (fun n c ->
            match c with
            | '0' -> 2L * n
            | '1' -> 2L * n + 1L
            | _ -> failwithf "invalid binary: %A" c)
        0
        cs

let splitAt (i : int64) (xs : 'x seq) : 'x seq * 'x seq =
    Seq.take (int i) xs, Seq.skip (int i) xs

let mapFst (f : 'a -> 'b) (t : 'a * 'c) : 'b * 'c =
    f (fst t), snd t

let rec parseLiteralWith (d1 : char seq) (s1 : char seq) : Packet Value * char seq =
    let (more, s2) = mapFst binaryToDecimal (splitAt 1 s1)
    let (chunk, s3) = splitAt 4 s2
    let d2 = Seq.append d1 chunk
    if more = 1 then
        parseLiteralWith d2 s3
    else
        Literal (binaryToDecimal d2), s3

let parseLiteral (s : char seq) : Packet Value * char seq =
    parseLiteralWith [] s

let rec parsePacketsWith (ps : Packet list) (s1 : char seq) : Packet list =
    if Seq.isEmpty s1 then
        List.rev ps
    else
        let (packet, s2) = parsePacket s1
        parsePacketsWith (packet :: ps) s2

and parsePackets (s : char seq) : Packet list =
    parsePacketsWith [] s

and parseNPacketsWith (ps : Packet list) (i : int64) (s1 : char seq) : Packet list * char seq =
    if i < 1 then
        List.rev ps, s1
    else
        let (packet, s2) = parsePacket s1
        parseNPacketsWith (packet :: ps) (i - 1L) s2

and parseNPackets (i : int64) (s : char seq) : Packet list * char seq =
    parseNPacketsWith [] i s

and parseOperator (s1 : char seq) : Packet Value * char seq =
    let (lengthTypeId, s2) = mapFst binaryToDecimal (splitAt 1 s1)
    if lengthTypeId = 0 then
        let (size, s3) = mapFst binaryToDecimal (splitAt 15 s2)
        let (bits, s4) = splitAt size s3
        Operator (parsePackets bits), s4
    else
        let (size, s3) = mapFst binaryToDecimal (splitAt 11 s2)
        mapFst Operator (parseNPackets size s3)

and parseValue (typeId : int64) (s: char seq) : Packet Value * char seq =
    match typeId with
    | 4L -> parseLiteral s
    | _ -> parseOperator s

and parsePacket (s1 : char seq) : Packet * char seq =
    let (version, s2) = mapFst binaryToDecimal (splitAt 3 s1)
    let (typeId, s3) = mapFst binaryToDecimal (splitAt 3 s2)
    let (value, s4) = parseValue typeId s3
    { version = version
    ; typeId = typeId
    ; value = value
    }, s4

let rec solve (p : Packet) : int64 =
    match p.typeId, p.value with
    | 0L, Operator ps ->
        let xs = Seq.map solve ps
        let y = Seq.sum xs
        y
    | 1L, Operator ps ->
        let xs = Seq.map solve ps
        let y = Seq.fold (*) 1L xs
        y
    | 2L, Operator ps ->
        let xs = Seq.map solve ps
        let y = Seq.min xs
        y
    | 3L, Operator ps ->
        let xs = Seq.map solve ps
        let y = Seq.max xs
        y
    | 4L, Literal x ->
        x
    | 5L, Operator [ x; y ] ->
        let x2 = solve x
        let y2 = solve y
        let z = if x2 > y2 then 1 else 0
        z
    | 6L, Operator [ x; y ] ->
        let x2 = solve x
        let y2 = solve y
        let z = if x2 < y2 then 1 else 0
        z
    | 7L, Operator [ x; y ] ->
        let x2 = solve x
        let y2 = solve y
        let z = if x2 = y2 then 1 else 0
        z
    | _ -> failwithf "unknown packet: %A" p

System.IO.File.ReadLines "16/input.txt"
|> Seq.map (fun line ->
    line
    |> Seq.collect hexadecimalToBinary
    |> parsePacket
    |> fst
    |> solve)
|> Seq.toList
|> printfn "%A"

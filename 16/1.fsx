type Value<'packet> =
    | Literal of int
    | Operator of 'packet seq

type Packet =
    { version : int
    ; typeId : int
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

let binaryToDecimal (cs : char seq) : int =
    Seq.fold
        (fun n c ->
            match c with
            | '0' -> 2 * n
            | '1' -> 2 * n + 1
            | _ -> failwithf "invalid binary: %A" c)
        0
        cs

let splitAt (i : int) (xs : 'x seq) : 'x seq * 'x seq =
    Seq.take i xs, Seq.skip i xs

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

let rec parsePacketsWith (ps : Packet seq) (s1 : char seq) : Packet seq =
    if Seq.isEmpty s1 then
        ps
    else
        let (packet, s2) = parsePacket s1
        parsePacketsWith (Seq.append ps (Seq.singleton packet)) s2

and parsePackets (s : char seq) : Packet seq =
    parsePacketsWith Seq.empty s

and parseNPacketsWith (ps : Packet seq) (i : int) (s1 : char seq) : Packet seq * char seq =
    if i < 1 then
        ps, s1
    else
        let (packet, s2) = parsePacket s1
        parseNPacketsWith (Seq.append ps (Seq.singleton packet)) (i - 1) s2

and parseNPackets (i : int) (s : char seq) : Packet seq * char seq =
    parseNPacketsWith Seq.empty i s

and parseOperator (s1 : char seq) : Packet Value * char seq =
    let (lengthTypeId, s2) = mapFst binaryToDecimal (splitAt 1 s1)
    if lengthTypeId = 0 then
        let (size, s3) = mapFst binaryToDecimal (splitAt 15 s2)
        let (bits, s4) = splitAt size s3
        Operator (parsePackets bits), s4
    else
        let (size, s3) = mapFst binaryToDecimal (splitAt 11 s2)
        mapFst Operator (parseNPackets size s3)

and parseValue (typeId : int) (s: char seq) : Packet Value * char seq =
    match typeId with
    | 4 -> parseLiteral s
    | _ -> parseOperator s

and parsePacket (s1 : char seq) : Packet * char seq =
    let (version, s2) = mapFst binaryToDecimal (splitAt 3 s1)
    let (typeId, s3) = mapFst binaryToDecimal (splitAt 3 s2)
    let (value, s4) = parseValue typeId s3
    { version = version
    ; typeId = typeId
    ; value = value
    }, s4

let rec solveWith (x : int) (p : Packet) : int =
    let y = x + p.version
    match p.value with
    | Literal _ -> y
    | Operator ps -> Seq.fold solveWith y ps

let solve (p : Packet) : int =
    solveWith 0 p

System.IO.File.ReadLines "16/input.txt"
|> Seq.map (fun line ->
    line
    |> Seq.collect hexadecimalToBinary
    |> parsePacket
    |> fst
    |> solve)
|> Seq.toList
|> printfn "%A"

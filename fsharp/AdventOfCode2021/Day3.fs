module Day3

type Bit =
    | Zero
    | One

let parseBit character =
    match character with
    | '0' -> Ok Zero
    | '1' -> Ok One
    | _ -> Error $"Invalid character: {character}"

let parseLine (line: string) =
    line
    |> Seq.map parseBit
    |> Seq.fold
        (fun list bitResult ->
            match bitResult with
            | Ok bit -> list @ [ bit ]
            | Error _ -> list)
        []
    |> Seq.toList

let parseInput (input: string) =
    StringUtils.splitLines input
    |> Seq.map parseLine
    |> Seq.toList

let getMostCommonBit (input: Bit list list) (column: int) =
    let bits =
        input
        |> Seq.map (fun row -> row.[column])
        |> Seq.groupBy id
        |> Seq.sortByDescending (fun (_, group) -> Seq.length group)
        |> Seq.toList
    bits.[0]

let partOne (input: string) : Result<string, string> = Ok ""

let partTwo (input: string) : Result<string, string> = Ok ""

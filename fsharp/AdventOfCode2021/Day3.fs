module Day3

open System

let parseInput (input: string) =
    StringUtils.splitLines input
    |> Seq.map
        (fun line ->
            line
            |> Seq.map (fun c -> int c - int '0')
            |> Seq.toList)
    |> Seq.toList

let getMostCommonBit (input: int list list) (column: int) : int =
    let groups =
        input
        |> Seq.map (fun row -> row.[column])
        |> Seq.groupBy id
        |> Seq.sortByDescending (fun (_, group) -> Seq.length group)
        |> Seq.toList

    match groups with
    | [ _, group1; _, group2 ] when Seq.length group1 = Seq.length group2 -> 1
    | _ -> groups |> Seq.map fst |> Seq.head

let getLeastCommonBit (input: int list list) (column: int) : int =
    let groups =
        input
        |> Seq.map (fun row -> row.[column])
        |> Seq.groupBy id
        |> Seq.sortBy (fun (_, group) -> Seq.length group)
        |> Seq.toList

    match groups with
    | [ _, group1; _, group2 ] when Seq.length group1 = Seq.length group2 -> 0
    | _ -> groups |> Seq.map fst |> Seq.head

let getGammaRate (input: int list list) =
    let rate =
        { 0 .. (Seq.length input.[0]) - 1 }
        |> Seq.map (getMostCommonBit input)
        |> Seq.fold (fun acc bit -> acc + string bit) ""

    Convert.ToInt32(rate, 2)

let getEpsilonRate (input: int list list) =
    let gammaRate = getGammaRate input

    let mask =
        { 0 .. (Seq.length input.[0]) - 1 }
        |> Seq.fold (fun acc _ -> acc <<< 1 ||| 0b1) 0b0

    ~~~gammaRate &&& mask

let partOne (input: string) : Result<string, string> =
    let parsedInput = parseInput input
    let gammaRate = getGammaRate parsedInput
    let epsilonRate = getEpsilonRate parsedInput
    let powerConsumption = gammaRate * epsilonRate
    Ok(string powerConsumption)

let getOxygenGeneratorRating (input: int list list) =
    let rec getRating (remainingInputs: int list list) (column: int) =
        match remainingInputs with
        | [ answer ] -> answer
        | _ ->
            let mostCommon = getMostCommonBit remainingInputs column

            let matches =
                remainingInputs
                |> Seq.filter (fun line -> line.[column] = mostCommon)
                |> Seq.toList

            getRating matches (column + 1)

    let rating =
        getRating input 0
        |> Seq.fold (fun acc bit -> acc + string bit) ""

    Convert.ToInt32(rating, 2)

let getCO2ScrubberRating (input: int list list) =
    let rec getRating (remainingInputs: int list list) (column: int) =
        match remainingInputs with
        | [ answer ] -> answer
        | _ ->
            let mostCommon = getLeastCommonBit remainingInputs column

            let matches =
                remainingInputs
                |> Seq.filter (fun line -> line.[column] = mostCommon)
                |> Seq.toList

            getRating matches (column + 1)

    let rating =
        getRating input 0
        |> Seq.fold (fun acc bit -> acc + string bit) ""

    Convert.ToInt32(rating, 2)

let partTwo (input: string) : Result<string, string> =
    let parsedInput = parseInput input
    let oxygenGeneratorRating = getOxygenGeneratorRating parsedInput
    let co2ScrubberRating = getCO2ScrubberRating parsedInput
    let lifeSupportRating = oxygenGeneratorRating * co2ScrubberRating
    Ok(string lifeSupportRating)

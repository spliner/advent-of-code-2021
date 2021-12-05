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
    input
    |> Seq.map (fun row -> row.[column])
    |> Seq.groupBy id
    |> Seq.sortByDescending (fun (_, group) -> Seq.length group)
    |> Seq.map fst
    |> Seq.head

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

let partTwo (input: string) : Result<string, string> = Ok ""

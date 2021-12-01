module Day1

let parseInput (input: string) =
    input.Split('\n')
        |> Seq.map (fun s -> s.Trim())
        |> Seq.map int
        |> Seq.toList

let countMeasurements (measurements: int list) =
    let rec countMeasurements' previous list =
        match list with
        | head :: tail when head > previous -> 1 + countMeasurements' head tail
        | head :: tail -> countMeasurements' head tail
        | [] -> 0

    countMeasurements' measurements.Head measurements.Tail

let partOne (input: string): Result<string, string> =
    let measurements = parseInput input
    match measurements with
    | _head :: _tail -> Ok (string (countMeasurements measurements))
    | _ -> Error "Invalid input"

let partTwo (input: string): Result<string, string> =
    Ok $"Part two got input {input}!"

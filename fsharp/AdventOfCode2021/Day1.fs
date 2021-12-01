module Day1

type SlidingWindow = int * int * int

let parseInput (input: string) =
    input.Split('\n')
        |> Seq.map (fun s -> s.Trim())
        |> Seq.map int

let countMeasurements (measurements: int list) =
    let rec countMeasurements' previous list =
        match list with
        | head :: tail when head > previous -> 1 + countMeasurements' head tail
        | head :: tail -> countMeasurements' head tail
        | [] -> 0

    countMeasurements' measurements.Head measurements.Tail

let partOne (input: string): Result<string, string> =
    let measurements = parseInput input |> Seq.toList
    match measurements with
    | _head :: _tail -> Ok (string (countMeasurements measurements))
    | _ -> Error "Invalid input"

let parseWindows (input: int list) =
    Seq.windowed 3 input
        |> Seq.map (fun array -> SlidingWindow (array.[0], array.[1], array.[2]))
        |> Seq.toList

let sum (window: SlidingWindow) =
    let (x, y, z) = window
    x + y + z

let countWindowMeasurements (windows: SlidingWindow list) =
    let sums = windows |> List.map sum
    countMeasurements sums

let partTwo (input: string): Result<string, string> =
    let windows = parseInput input |> Seq.toList |> parseWindows
    match windows with
    | _head :: _tail -> Ok (string (countWindowMeasurements windows))
    | _ -> Error "Invalid input"

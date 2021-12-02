module Day1

type Measurement = Measurement of int
type SlidingWindow = Measurement * Measurement * Measurement

let parseInput (input: string) =
    input.Split('\n')
    |> Seq.map (fun s -> s.Trim())
    |> Seq.map int
    |> Seq.map (fun m -> Measurement(m))

let countMeasurements (measurements: Measurement list) =
    let rec countMeasurements' list previous =
        match list with
        | head :: tail when head > previous -> 1 + countMeasurements' tail head
        | head :: tail -> countMeasurements' tail head
        | [] -> 0

    countMeasurements' measurements.Tail measurements.Head

let partOne (input: string) : Result<string, string> =
    let measurements = parseInput input |> Seq.toList

    match measurements with
    | _head :: _tail -> Ok(string (countMeasurements measurements))
    | _ -> Error "Invalid input"

let parseWindows (input: Measurement list) =
    Seq.windowed 3 input
    |> Seq.map (fun array -> SlidingWindow(array.[0], array.[1], array.[2]))
    |> Seq.toList

let sum (window: SlidingWindow) =
    let (Measurement x, Measurement y, Measurement z) = window
    Measurement(x + y + z)

let countWindowMeasurements (windows: SlidingWindow list) =
    let sums = windows |> List.map sum
    countMeasurements sums

let partTwo (input: string) : Result<string, string> =
    let windows =
        parseInput input |> Seq.toList |> parseWindows

    match windows with
    | _head :: _tail -> Ok(string (countWindowMeasurements windows))
    | _ -> Error "Invalid input"

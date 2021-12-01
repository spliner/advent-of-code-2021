open System
open System.IO

let (>=>) f1 f2 arg =
    match f1 arg with
    | Ok data -> f2 data
    | Error e -> Error e

type Argument = { Day: int; Part: int; Input: string }

let parseArgs () =
    let args = Environment.GetCommandLineArgs()

    if Array.length args = 4 then
        let path = args.[3]
        let input = File.ReadAllText path

        Ok
            { Day = int args.[1]
              Part = int args.[2]
              Input = input }
    else
        Error "Invalid number of arguments"

let invokeRunner argument =
    let getRunner argument =
        match (argument.Day, argument.Part) with
        | 1, 1 -> Ok(Day1.partOne)
        | 1, 2 -> Ok(Day1.partTwo)
        | day, part -> Error $"Could not find a runner for day {day} part {part}"

    match getRunner argument with
    | Ok runner -> runner argument.Input
    | Error e -> Error e

let getResult = parseArgs >=> invokeRunner

match getResult () with
| Ok r -> printf $"Success: {r}"
| Error e -> printf $"Error: {e}"

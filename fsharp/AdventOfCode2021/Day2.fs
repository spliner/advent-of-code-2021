module Day2

type Command =
    | Forward of int
    | Down of int
    | Up of int

type State = { Position: int; Depth: int; Aim: int }

let parseCommand (rawCommand: string) =
    let split = rawCommand.Split(' ')

    match split with
    | [| commandName; unit |] ->
        let unit = int unit

        match commandName with
        | "forward" -> Ok(Forward(unit))
        | "down" -> Ok(Down(unit))
        | "up" -> Ok(Up(unit))
        | _ -> Error $"Invalid command name: {commandName}"
    | _ -> Error $"Invalid command: {rawCommand}"

let parseInput (input: string) =
    input.Trim().Split('\n')
    |> Seq.map (fun s -> s.Trim())
    |> Seq.map parseCommand
    |> Seq.fold
        (fun list command ->
            match command with
            | Ok c -> list @ [ c ]
            | _ -> list)
        []
    |> Seq.toList

let nextState state command =
    match command with
    | Forward units ->
        { state with
              Position = state.Position + units }
    | Up units ->
        { state with
              Depth = state.Depth - units }
    | Down units ->
        { state with
              Depth = state.Depth + units }

let runCourse (commands: Command list) =
    let finalState =
        commands
        |> Seq.fold nextState { Position = 0; Depth = 0; Aim = 0 }

    finalState.Position * finalState.Depth

let partOne (input: string) : Result<string, string> =
    let commands = parseInput input
    let result = runCourse commands
    Ok $"{result}"

let nextStateWithAim state command =
    match command with
    | Down units -> { state with Aim = state.Aim + units }
    | Up units -> { state with Aim = state.Aim - units }
    | Forward units ->
        { state with
              Position = state.Position + units
              Depth = state.Depth + state.Aim * units }

let runCourseWithAim (commands: Command list) =
    let finalState =
        commands
        |> Seq.fold nextStateWithAim { Position = 0; Depth = 0; Aim = 0 }

    finalState.Position * finalState.Depth

let partTwo (input: string) : Result<string, string> =
    let commands = parseInput input
    let result = runCourseWithAim commands
    Ok $"{result}"

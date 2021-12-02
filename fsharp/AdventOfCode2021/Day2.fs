module Day2

type Command =
    | Forward of int
    | Down of int
    | Up of int

type State = { Position: int; Depth: int }

let parseCommand (rawCommand: string) =
    let split = rawCommand.Split(' ')

    if Array.length split <> 2 then
        Error $"Invalid command: {rawCommand}"
    else
        let commandName = split.[0]
        let unit = int split.[1]

        match commandName with
        | "forward" -> Ok(Forward(unit))
        | "down" -> Ok(Down(unit))
        | "up" -> Ok(Up(unit))
        | _ -> Error $"Invalid command name: {commandName}"

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
        |> Seq.fold nextState { Position = 0; Depth = 0 }

    finalState.Position * finalState.Depth

let partOne (input: string) : Result<string, string> =
    let commands = parseInput(input)
    let result = runCourse commands
    Ok $"{result}"

let partTwo (input: string) : Result<string, string> = Ok $"Got {input}!"

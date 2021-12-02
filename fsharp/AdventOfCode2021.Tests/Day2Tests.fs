module Day2Tests

open Xunit

[<Fact>]
let ``Parse forward command should correctly parse`` () =
    let rawCommand = "forward 12"
    let command = Day2.parseCommand rawCommand
    Assert.Equal(Ok(Day2.Forward(12)), command)

[<Fact>]
let ``Parse down command should correctly parse`` () =
    let rawCommand = "down 10"
    let command = Day2.parseCommand rawCommand
    Assert.Equal(Ok(Day2.Down(10)), command)

[<Fact>]
let ``Parse up command should correctly parse`` () =
    let rawCommand = "up 5"
    let command = Day2.parseCommand rawCommand
    Assert.Equal(Ok(Day2.Up(5)), command)

[<Fact>]
let ``Parse invalid string should return error`` () =
    let rawCommand = "up 5 123"
    let command = Day2.parseCommand rawCommand
    Assert.Equal(Error("Invalid command: up 5 123"), command)

[<Fact>]
let ``Parse invalid command name should return error`` () =
    let rawCommand = "foo 5"
    let command = Day2.parseCommand rawCommand
    Assert.Equal(Error("Invalid command name: foo"), command)

[<Fact>]
let ``Parse example input should correctly parse`` () =
    let rawInput =
        "forward 5
down 5
forward 8
up 3
down 8
forward 2"

    let parsedInput = Day2.parseInput rawInput

    let expected =
        [ Day2.Forward 5
          Day2.Down 5
          Day2.Forward 8
          Day2.Up 3
          Day2.Down 8
          Day2.Forward 2 ]

    Assert.Equal<Day2.Command>(expected, parsedInput)

[<Fact>]
let ``Parse raw input with invalid commands should return only valid commands`` () =
    let rawInput =
        "forward2 5
down 5 123
forward
up 3
down 8
forward 2"

    let parsedInput = Day2.parseInput rawInput

    let expected =
        [ Day2.Up 3
          Day2.Down 8
          Day2.Forward 2 ]

    Assert.Equal<Day2.Command>(expected, parsedInput)

[<Fact>]
let ``Next state with Forward command adds units to position`` () =
    let state = { Day2.Position = 10; Day2.Depth = 10 }
    let command = Day2.Forward 10

    let nextState = Day2.nextState state command

    let expected = { Day2.Position = 20; Day2.Depth = 10 }

    Assert.Equal(expected, nextState)

[<Fact>]
let ``Next state with Up command subtracts units from depth`` () =
    let state = { Day2.Position = 10; Day2.Depth = 10 }
    let command = Day2.Up 10

    let nextState = Day2.nextState state command

    let expected = { Day2.Position = 10; Day2.Depth = 0 }

    Assert.Equal(expected, nextState)

[<Fact>]
let ``Next state with Down command adds units to depth`` () =
    let state = { Day2.Position = 10; Day2.Depth = 10 }
    let command = Day2.Down 10

    let nextState = Day2.nextState state command

    let expected = { Day2.Position = 10; Day2.Depth = 20 }

    Assert.Equal(expected, nextState)

[<Fact>]
let ``Run course for example input should return 150`` () =
    let commands =
        [ Day2.Forward 5
          Day2.Down 5
          Day2.Forward 8
          Day2.Up 3
          Day2.Down 8
          Day2.Forward 2 ]

    let result = Day2.runCourse commands

    Assert.Equal(150, result)

[<Fact>]
let ``Run course with aim for example input should return 900`` () =
    let commands =
        [ Day2.Forward 5
          Day2.Down 5
          Day2.Forward 8
          Day2.Up 3
          Day2.Down 8
          Day2.Forward 2 ]

    let result = Day2.runCourseWithAim commands

    Assert.Equal(900, result)

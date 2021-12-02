module Day1Tests

open Xunit

[<Fact>]
let ``Count measurements should return 7 for example input`` () =
    let measurements =
        [ 199
          200
          208
          210
          200
          207
          240
          269
          260
          263 ]
        |> List.map Day1.Measurement

    let count = Day1.countMeasurements measurements
    Assert.Equal(7, count)

[<Fact>]
let ``Parse windows should return 8 windows`` () =
    let measurements =
        [ 199
          200
          208
          210
          200
          207
          240
          269
          260
          263 ]
        |> List.map Day1.Measurement

    let windows = Day1.parseWindows measurements

    let expected =
        [ Day1.SlidingWindow(Day1.Measurement 199, Day1.Measurement 200, Day1.Measurement 208)
          Day1.SlidingWindow(Day1.Measurement 200, Day1.Measurement 208, Day1.Measurement 210)
          Day1.SlidingWindow(Day1.Measurement 208, Day1.Measurement 210, Day1.Measurement 200)
          Day1.SlidingWindow(Day1.Measurement 210, Day1.Measurement 200, Day1.Measurement 207)
          Day1.SlidingWindow(Day1.Measurement 200, Day1.Measurement 207, Day1.Measurement 240)
          Day1.SlidingWindow(Day1.Measurement 207, Day1.Measurement 240, Day1.Measurement 269)
          Day1.SlidingWindow(Day1.Measurement 240, Day1.Measurement 269, Day1.Measurement 260)
          Day1.SlidingWindow(Day1.Measurement 269, Day1.Measurement 260, Day1.Measurement 263) ]

    Assert.Equal<Day1.SlidingWindow>(expected, windows)

[<Fact>]
let ``Sum of a sliding window should return correct value`` () =
    let window =
        Day1.SlidingWindow(Day1.Measurement 199, Day1.Measurement 200, Day1.Measurement 208)

    let sum = Day1.sum window
    Assert.Equal(Day1.Measurement 607, sum)

[<Fact>]
let ``Count window measurements should return 5 for example input`` () =
    let windows =
        [ Day1.SlidingWindow(Day1.Measurement 199, Day1.Measurement 200, Day1.Measurement 208)
          Day1.SlidingWindow(Day1.Measurement 200, Day1.Measurement 208, Day1.Measurement 210)
          Day1.SlidingWindow(Day1.Measurement 208, Day1.Measurement 210, Day1.Measurement 200)
          Day1.SlidingWindow(Day1.Measurement 210, Day1.Measurement 200, Day1.Measurement 207)
          Day1.SlidingWindow(Day1.Measurement 200, Day1.Measurement 207, Day1.Measurement 240)
          Day1.SlidingWindow(Day1.Measurement 207, Day1.Measurement 240, Day1.Measurement 269)
          Day1.SlidingWindow(Day1.Measurement 240, Day1.Measurement 269, Day1.Measurement 260)
          Day1.SlidingWindow(Day1.Measurement 269, Day1.Measurement 260, Day1.Measurement 263) ]

    let measurements = Day1.countWindowMeasurements windows
    Assert.Equal(5, measurements)

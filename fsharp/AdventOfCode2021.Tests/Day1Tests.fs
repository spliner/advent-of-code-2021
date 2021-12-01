module Tests

open Xunit

[<Fact>]
let ``Count measurements should return 7 for example input`` () =
    let measurements = [ 199; 200; 208; 210; 200; 207; 240; 269; 260; 263; ]
    let count = Day1.countMeasurements measurements
    Assert.Equal(7, count);

[<Fact>]
let ``Parse windows should return 8 windows`` () =
    let measurements = [ 199; 200; 208; 210; 200; 207; 240; 269; 260; 263; ]
    let windows = Day1.parseWindows measurements

    let expected = [
        Day1.SlidingWindow(199, 200, 208)
        Day1.SlidingWindow(200, 208, 210)
        Day1.SlidingWindow(208, 210, 200)
        Day1.SlidingWindow(210, 200, 207)
        Day1.SlidingWindow(200, 207, 240)
        Day1.SlidingWindow(207, 240, 269)
        Day1.SlidingWindow(240, 269, 260)
        Day1.SlidingWindow(269, 260, 263)
    ]

    Assert.Equal<Day1.SlidingWindow>(expected, windows);

[<Fact>]
let ``Sum of a sliding window should return correct value`` () =
    let window = Day1.SlidingWindow(199, 200, 208)
    let sum = Day1.sum window
    Assert.Equal(607, sum)

[<Fact>]
let ``Count window measurements should return 5 for example input`` () =
    let windows = [
        Day1.SlidingWindow(199, 200, 208)
        Day1.SlidingWindow(200, 208, 210)
        Day1.SlidingWindow(208, 210, 200)
        Day1.SlidingWindow(210, 200, 207)
        Day1.SlidingWindow(200, 207, 240)
        Day1.SlidingWindow(207, 240, 269)
        Day1.SlidingWindow(240, 269, 260)
        Day1.SlidingWindow(269, 260, 263)
    ]
    let measurements = Day1.countWindowMeasurements windows
    Assert.Equal(5, measurements);

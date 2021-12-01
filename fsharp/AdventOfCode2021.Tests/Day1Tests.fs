module Tests

open Xunit

let measurements = [ 199; 200; 208; 210; 200; 207; 240; 269; 260; 263; ]

[<Fact>]
let ``Count measurements should return 7 for example input`` () =
    let count = Day1.countMeasurements measurements
    Assert.Equal(7, count);

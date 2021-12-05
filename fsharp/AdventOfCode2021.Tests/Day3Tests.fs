module Day3Tests

open Xunit

let input =
    "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010"

let expectedParsedInput =
    [ [ 0; 0; 1; 0; 0 ]
      [ 1; 1; 1; 1; 0 ]
      [ 1; 0; 1; 1; 0 ]
      [ 1; 0; 1; 1; 1 ]
      [ 1; 0; 1; 0; 1 ]
      [ 0; 1; 1; 1; 1 ]
      [ 0; 0; 1; 1; 1 ]
      [ 1; 1; 1; 0; 0 ]
      [ 1; 0; 0; 0; 0 ]
      [ 1; 1; 0; 0; 1 ]
      [ 0; 0; 0; 1; 0 ]
      [ 0; 1; 0; 1; 0 ] ]

[<Fact>]
let ``Parse example input should return int matrix`` () =
    let parsedInput = Day3.parseInput input
    Assert.Equal<int list>(expectedParsedInput, parsedInput)

[<Theory>]
[<InlineData(0, 1)>]
[<InlineData(1, 0)>]
[<InlineData(2, 1)>]
[<InlineData(3, 1)>]
[<InlineData(4, 0)>]
let ``Most common bit of example input column should match expected value`` (column: int, expected: int) =
    let mostCommon =
        Day3.getMostCommonBit expectedParsedInput column

    Assert.Equal(expected, mostCommon)

[<Fact>]
let ``Gamma rate of example input should be 22`` () =
    let gammaRate = Day3.getGammaRate expectedParsedInput
    Assert.Equal(22, gammaRate)

[<Fact>]
let ``Epsilon rate of example input should be 9`` () =
    let gammaRate = Day3.getEpsilonRate expectedParsedInput
    Assert.Equal(9, gammaRate)

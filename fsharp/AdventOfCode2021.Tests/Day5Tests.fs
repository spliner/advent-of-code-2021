module Day5Tests

open Xunit

type AllPoints() =
    static member TestData: obj [] list =
        [ [| Day5.Point(0, 9)
             Day5.Point(5, 9)
             Set.ofList [ Day5.Point(0, 9)
                          Day5.Point(1, 9)
                          Day5.Point(2, 9)
                          Day5.Point(3, 9)
                          Day5.Point(4, 9)
                          Day5.Point(5, 9) ] |]
          [| Day5.Point(9, 4)
             Day5.Point(3, 4)
             Set.ofList [ Day5.Point(3, 4)
                          Day5.Point(4, 4)
                          Day5.Point(5, 4)
                          Day5.Point(6, 4)
                          Day5.Point(7, 4)
                          Day5.Point(8, 4)
                          Day5.Point(9, 4) ] |]

          [| Day5.Point(1, 1)
             Day5.Point(1, 3)
             Set.ofList [ Day5.Point(1, 1)
                          Day5.Point(1, 2)
                          Day5.Point(1, 3) ] |] ]

    [<Theory>]
    [<MemberData(nameof AllPoints.TestData)>]
    member _.``Create line should contain all points`` startPoint endPoint expectedPoints =
        let line = Day5.Line.create startPoint endPoint

        Assert.Equal(startPoint, line.Start)
        Assert.Equal(endPoint, line.End)

        Assert.Equal<Day5.Point>(expectedPoints, line.All)

[<Fact>]
let ``Parse example input should return correct points`` () =
    let input =
        "0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2"

    let parsedInput = Day5.parseInput input

    let points =
        parsedInput
        |> List.map (fun line -> line.Start, line.End)

    let expectedPoints =
        [ Day5.Point(0, 9), Day5.Point(5, 9)
          Day5.Point(8, 0), Day5.Point(0, 8)
          Day5.Point(9, 4), Day5.Point(3, 4)
          Day5.Point(2, 2), Day5.Point(2, 1)
          Day5.Point(7, 0), Day5.Point(7, 4)
          Day5.Point(6, 4), Day5.Point(2, 0)
          Day5.Point(0, 9), Day5.Point(2, 9)
          Day5.Point(3, 4), Day5.Point(1, 4)
          Day5.Point(0, 0), Day5.Point(8, 8)
          Day5.Point(5, 5), Day5.Point(8, 2) ]

    Assert.Equal<Day5.Point * Day5.Point>(expectedPoints, points)

[<Fact>]
let ``Get intersection should return expected value`` () =
    let line1 =
        Day5.Line.create (Day5.Point(0, 9)) (Day5.Point(5, 9))

    let line2 =
        Day5.Line.create (Day5.Point(0, 9)) (Day5.Point(2, 9))

    let intersection = Day5.Line.getIntersection line1 line2

    let expectedIntersection =
        Seq.ofList [ Day5.Point(0, 9)
                     Day5.Point(1, 9)
                     Day5.Point(2, 9) ]

    Assert.Equal<Day5.Point>(expectedIntersection, intersection)

[<Fact>]
let ``Count overlaps of example input should return 5`` () =
    let lines =
        [ Day5.Point(0, 9), Day5.Point(5, 9)
          Day5.Point(8, 0), Day5.Point(0, 8)
          Day5.Point(9, 4), Day5.Point(3, 4)
          Day5.Point(2, 2), Day5.Point(2, 1)
          Day5.Point(7, 0), Day5.Point(7, 4)
          Day5.Point(6, 4), Day5.Point(2, 0)
          Day5.Point(0, 9), Day5.Point(2, 9)
          Day5.Point(3, 4), Day5.Point(1, 4)
          Day5.Point(0, 0), Day5.Point(8, 8)
          Day5.Point(5, 5), Day5.Point(8, 2) ]
        |> List.map (fun (startPoint, endPoint) -> Day5.Line.create startPoint endPoint)

    let overlaps = Day5.countOverlaps lines

    Assert.Equal(5, overlaps)

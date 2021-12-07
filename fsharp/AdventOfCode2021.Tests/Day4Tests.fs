module Day4Tests

open Xunit

[<Fact>]
let ``Bingo board with winning row should be won`` () =
    let numbers =
        [ [ 22; 13; 17; 11; 0 ]
          [ 8; 2; 23; 4; 24 ]
          [ 21; 9; 14; 16; 7 ]
          [ 6; 10; 3; 18; 5 ]
          [ 1; 12; 20; 15; 19 ] ]

    let board = Day4.BingoBoard.create numbers
    let drawnNumbers = [ 22; 13; 17; 11; 0 ]

    let isWon = Day4.BingoBoard.isWon drawnNumbers board

    Assert.Equal(true, isWon)

[<Fact>]
let ``Bingo board without winning row should not be won`` () =
    let numbers =
        [ [ 22; 13; 17; 11; 0 ]
          [ 8; 2; 23; 4; 24 ]
          [ 21; 9; 14; 16; 7 ]
          [ 6; 10; 3; 18; 5 ]
          [ 1; 12; 20; 15; 19 ] ]

    let board = Day4.BingoBoard.create numbers
    let drawnNumbers = [ 22; 13; 17; 11; 10 ]

    let isWon = Day4.BingoBoard.isWon drawnNumbers board

    Assert.Equal(false, isWon)

[<Fact>]
let ``Bingo board with winning column should be won`` () =
    let numbers =
        [ [ 22; 13; 17; 11; 0 ]
          [ 8; 2; 23; 4; 24 ]
          [ 21; 9; 14; 16; 7 ]
          [ 6; 10; 3; 18; 5 ]
          [ 1; 12; 20; 15; 19 ] ]

    let board = Day4.BingoBoard.create numbers
    let drawnNumbers = [ 10; 12; 9; 13; 2; 15 ]

    let isWon = Day4.BingoBoard.isWon drawnNumbers board

    Assert.Equal(true, isWon)

[<Fact>]
let ``Bingo board without winning column should not be won`` () =
    let numbers =
        [ [ 22; 13; 17; 11; 0 ]
          [ 8; 2; 23; 4; 24 ]
          [ 21; 9; 14; 16; 7 ]
          [ 6; 10; 3; 18; 5 ]
          [ 1; 12; 20; 15; 19 ] ]

    let board = Day4.BingoBoard.create numbers
    let drawnNumbers = [ 10; 1; 9; 13; 2; 15 ]

    let isWon = Day4.BingoBoard.isWon drawnNumbers board

    Assert.Equal(false, isWon)

[<Fact>]
let ``Get score of example winner board should return 4512`` () =
    let numbers =
        [ [ 14; 21; 17; 24; 4 ]
          [ 10; 16; 15; 9; 19 ]
          [ 18; 8; 23; 26; 20 ]
          [ 22; 11; 13; 6; 5 ]
          [ 2; 0; 12; 3; 7 ] ]

    let board = Day4.BingoBoard.create numbers

    let drawNumbers =
        [ 7
          4
          9
          5
          11
          17
          23
          2
          0
          14
          21
          24 ]

    let score =
        Day4.BingoBoard.getScore board drawNumbers

    Assert.Equal(4512, score)

[<Fact>]
let ``Parse input should return numbers to draw and boards`` () =
    let input =
        "26,55,7,40,56,34,58,90

62  5 77 94 75
59 10 23 44 29
93 91 63 51 74
22 14 15  2 55
78 18 95 58 57

43 12 34 37 11
84 69 52 38 68
40 89 67 98 16
47 59 96 63 95
 3 21 58 75 20"

    let parsedInput = Day4.parseInput input
    let numbersToDraw, boards = parsedInput

    let expectedNumbersToDraw = [ 26; 55; 7; 40; 56; 34; 58; 90 ]

    let expectedBoards =
        [ [ [ 62; 5; 77; 94; 75 ]
            [ 59; 10; 23; 44; 29 ]
            [ 93; 91; 63; 51; 74 ]
            [ 22; 14; 15; 2; 55 ]
            [ 78; 18; 95; 58; 57 ] ]
          [ [ 43; 12; 34; 37; 11 ]
            [ 84; 69; 52; 38; 68 ]
            [ 40; 89; 67; 98; 16 ]
            [ 47; 59; 96; 63; 95 ]
            [ 3; 21; 58; 75; 20 ] ] ]
        |> List.map Day4.BingoBoard.create

    Assert.Equal<int>(expectedNumbersToDraw, numbersToDraw)
    Assert.Equal<Day4.BingoBoard.T>(expectedBoards, boards)

[<Fact>]
let ``Get winning score should return 4512 for example input`` () =
    let numbersToDraw =
        [ 7
          4
          9
          5
          11
          17
          23
          2
          0
          14
          21
          24
          10
          16
          13
          6
          15
          25
          12
          22
          18
          20
          8
          19
          3
          26
          1 ]

    let boards =
        [ [ [ 22; 13; 17; 11; 0 ]
            [ 8; 2; 23; 4; 24 ]
            [ 21; 9; 14; 16; 7 ]
            [ 6; 10; 3; 18; 5 ]
            [ 1; 12; 20; 15; 19 ] ]
          [ [ 3; 15; 0; 2; 22 ]
            [ 9; 18; 13; 17; 5 ]
            [ 19; 8; 7; 25; 23 ]
            [ 20; 11; 10; 24; 4 ]
            [ 14; 21; 16; 12; 6 ] ]
          [ [ 14; 21; 17; 24; 4 ]
            [ 10; 16; 15; 9; 19 ]
            [ 18; 8; 23; 26; 20 ]
            [ 22; 11; 13; 6; 5 ]
            [ 2; 0; 12; 3; 7 ] ] ]
        |> List.map Day4.BingoBoard.create

    let score =
        Day4.getWinningScore numbersToDraw boards

    Assert.Equal(4512, score)

[<Fact>]
let ``Get last winning score should return 1924 for example input`` () =
    let numbersToDraw =
        [ 7
          4
          9
          5
          11
          17
          23
          2
          0
          14
          21
          24
          10
          16
          13
          6
          15
          25
          12
          22
          18
          20
          8
          19
          3
          26
          1 ]

    let boards =
        [ [ [ 22; 13; 17; 11; 0 ]
            [ 8; 2; 23; 4; 24 ]
            [ 21; 9; 14; 16; 7 ]
            [ 6; 10; 3; 18; 5 ]
            [ 1; 12; 20; 15; 19 ] ]
          [ [ 3; 15; 0; 2; 22 ]
            [ 9; 18; 13; 17; 5 ]
            [ 19; 8; 7; 25; 23 ]
            [ 20; 11; 10; 24; 4 ]
            [ 14; 21; 16; 12; 6 ] ]
          [ [ 14; 21; 17; 24; 4 ]
            [ 10; 16; 15; 9; 19 ]
            [ 18; 8; 23; 26; 20 ]
            [ 22; 11; 13; 6; 5 ]
            [ 2; 0; 12; 3; 7 ] ] ]
        |> List.map Day4.BingoBoard.create

    let score =
        Day4.getLastWinningScore numbersToDraw boards

    Assert.Equal(1924, score)

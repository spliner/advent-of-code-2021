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
    let drawnNumbers = Set.ofList [ 22; 13; 17; 11; 0 ]

    let isWon = Day4.BingoBoard.isWon board drawnNumbers

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
    let drawnNumbers = Set.ofList [ 22; 13; 17; 11; 10 ]

    let isWon = Day4.BingoBoard.isWon board drawnNumbers

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
    let drawnNumbers = Set.ofList [ 10; 12; 9; 13; 2; 15 ]

    let isWon = Day4.BingoBoard.isWon board drawnNumbers

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
    let drawnNumbers = Set.ofList [ 10; 1; 9; 13; 2; 15 ]

    let isWon = Day4.BingoBoard.isWon board drawnNumbers

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
    let drawNumbers = Set.ofList [ 7; 4; 9; 5; 11; 17; 23; 2; 0; 14; 21; 24 ]

    let foo = Day4.BingoBoard.getScore board drawNumbers

    Assert.Equal(4512, foo)

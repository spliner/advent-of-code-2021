module Day4

open System

module BingoBoard =
    type T = { Numbers: int list list }

    let create numbers = { Numbers = numbers }

    let private rows { Numbers = numbers } = Seq.ofList numbers

    let private columns { Numbers = numbers } =
        let length = List.length numbers

        let result =
            seq { 0 .. length - 1 }
            |> Seq.map
                (fun column ->
                    numbers
                    |> Seq.map (fun row -> row.[column])
                    |> Seq.toList)

        result

    let isWon (drawnNumbers: int list) (board: T) =
        let won numbers =
            numbers
            |> Seq.forall (fun n -> List.contains n drawnNumbers)

        let hasWinningRow =
            rows board
            |> Seq.filter won
            |> (fun seq -> not (Seq.isEmpty seq))

        let hasWinningColumn =
            columns board
            |> Seq.filter won
            |> (fun seq -> not (Seq.isEmpty seq))

        hasWinningRow || hasWinningColumn

    let getScore (board: T) (drawnNumbers: int list) =
        let { Numbers = numbers } = board

        numbers
        |> Seq.collect id
        |> Seq.filter (fun number -> not (List.contains number drawnNumbers))
        |> Seq.sum
        |> (fun sum -> sum * Seq.last drawnNumbers)

let parseInput (input: string) =
    let lines =
        StringUtils.splitLines input |> Seq.toList

    let numbersToDraw =
        lines.[0].Split(',')
        |> Seq.map (fun s -> s.Trim())
        |> Seq.map int
        |> Seq.toList

    let boards =
        lines
        |> Seq.skip 2
        |> Seq.chunkBySize 6
        |> Seq.map
            (fun chunk ->
                let numbers =
                    chunk
                    |> Seq.take 5
                    |> Seq.map
                        (fun line ->
                            line.Split(' ')
                            |> Seq.map (fun s -> s.Trim())
                            |> Seq.filter (fun s -> not (String.IsNullOrEmpty(s)))
                            |> Seq.map int
                            |> Seq.toList)
                    |> Seq.toList

                BingoBoard.create numbers)
        |> Seq.toList

    numbersToDraw, boards

let getWinningScore (numbersToDraw: int list) (boards: BingoBoard.T list) =
    let rec loop (drawn: int list) (currentIndex: int) =
        let newNumber = numbersToDraw.[currentIndex]
        let drawn = drawn @ [ newNumber ]

        let winner =
            boards
            |> Seq.filter (BingoBoard.isWon drawn)
            |> Seq.tryHead

        match winner with
        | Some board -> BingoBoard.getScore board drawn
        | None -> loop drawn (currentIndex + 1)

    loop List.empty 0

let partOne (input: string) : Result<string, string> =
    let result =
        input
        |> parseInput
        |> (fun parsedInput ->
            let numbersToDraw, boards = parsedInput
            getWinningScore numbersToDraw boards)

    Ok(string result)

let partTwo (input: string) : Result<string, string> = Ok ""

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

    let isWon (board: T) (drawnNumbers: int Set) =
        let won numbers =
            numbers
            |> Seq.forall (fun n -> Set.contains n drawnNumbers)

        let hasWinningRow =
            rows board
            |> Seq.filter won
            |> (fun seq -> not (Seq.isEmpty seq))

        let hasWinningColumn =
            columns board
            |> Seq.filter won
            |> (fun seq -> not (Seq.isEmpty seq))

        hasWinningRow || hasWinningColumn

    let getScore (board: T) (drawnNumbers: int Set) =
        let { Numbers = numbers } = board

        numbers
        |> Seq.collect id
        |> Seq.filter (fun number -> not (drawnNumbers.Contains number))
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

    numbersToDraw, boards


let partOne (input: string) : Result<string, string> = Ok ""

let partTwo (input: string) : Result<string, string> = Ok ""

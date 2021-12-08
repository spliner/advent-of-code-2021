module Day5

type Point = int * int

module Line =
    type T =
        { Start: Point
          End: Point
          All: Point Set }

    let private isVertical' startPoint endPoint =
        let x1, _ = startPoint
        let x2, _ = endPoint
        x1 = x2

    let isVertical { Start = startPoint; End = endPoint } = isVertical' startPoint endPoint

    let isHorizontal' startPoint endPoint =
        let _, y1 = startPoint
        let _, y2 = endPoint
        y1 = y2

    let isHorizontal { Start = startPoint; End = endPoint } = isHorizontal' startPoint endPoint

    let create (startPoint: Point) (endPoint: Point) =
        let x1, y1 = startPoint
        let x2, y2 = endPoint

        let allPoints =
            if isVertical' startPoint endPoint then
                let startY, endY = if y1 > y2 then y2, y1 else y1, y2
                [ for y in startY .. endY -> (x1, y) ]
            elif isHorizontal' startPoint endPoint then
                let startX, endX = if x1 > x2 then x2, x1 else x1, x2
                [ for x in startX .. endX -> (x, y1) ]
            else
                List.empty

        { Start = startPoint
          End = endPoint
          All = Set.ofList allPoints }

    let getIntersection (line1: T) (line2: T) =
        let { All = allPoints1 } = line1
        let { All = allPoints2 } = line2

        Set.intersect allPoints1 allPoints2

let parseInput (input: string) =
    StringUtils.splitLines input
    |> Seq.map
        (fun line ->
            let startPoint, endPoint =
                match line.Split(" -> ") with
                | [| p1; p2 |] ->
                    let getCoordinates (part: string) =
                        let parts =
                            part.Split(',') |> Seq.map int |> Seq.toList

                        parts.[0], parts.[1]

                    (getCoordinates p1), (getCoordinates p2)
                | _ -> (0, 0), (0, 0)

            Line.create startPoint endPoint)
    |> Seq.toList

let countOverlaps (lines: Line.T list) =
    lines
    |> Seq.mapi
        (fun index line ->
            lines
            |> Seq.skip (index + 1)
            |> Seq.fold
                (fun acc otherLine ->
                    let intersection = Line.getIntersection line otherLine
                    acc + intersection)
                Set.empty)
    |> Seq.fold (fun acc points -> acc + points) Set.empty
    |> Seq.length

let partOne (input: string) : Result<string, string> =
    let result = parseInput input |> countOverlaps
    Ok(string result)

let partTwo (input: string) : Result<string, string> = Ok ""

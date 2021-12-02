module StringUtils

let splitLines (string: string) =
    string.Trim().Split('\n')
    |> Seq.map (fun s -> s.Trim())

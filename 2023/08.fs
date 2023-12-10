module aoc_2023_08

let regex pattern s =
    match (new System.Text.RegularExpressions.Regex(pattern)).Match(s) with
    | null -> []
    | m ->
        m.Groups
        |> List.ofSeq
        |> List.tail
        |> List.map (fun g -> g.Value)

let toCharList (s: string) =
    s.ToCharArray()
    |> List.ofArray

let parseLine line =
    match line |> regex @"(\S+)\s+=\s+\((\S+),\s+(\S+)\)" with
    | [a; b; c] -> (a, (b, c))
    | _ -> failwith "Invalid input line"

let parseInput lines =
    match lines with
    | head :: tail -> (head |> toCharList, tail |> List.map parseLine)
    | _ -> failwith "Invalid input"

let readInput =
    System.IO.File.ReadAllLines
    >> List.ofArray
    >> List.filter (System.String.IsNullOrWhiteSpace >> not)
    >> parseInput

let solvePart1 (instructions, paths) =
    let map = paths |> Map.ofList
    let rec countRoute acc location instructionsLeft =
        match location with
        | "ZZZ" -> acc
        | location ->
            match instructionsLeft with
            | head :: tail ->
                let (left, right) =
                    map
                    |> Map.find location
                match head with
                | 'L' -> countRoute (acc + 1) left tail
                | 'R' -> countRoute (acc + 1) right tail
                | _ -> failwith "invalid instruction"
            | [] -> countRoute acc location instructions
    countRoute 0 "AAA" instructions

let solvePart2 = solvePart1

let solve input =
    let config = readInput input
    config |> solvePart1 |> printfn "%A"
    config |> solvePart2 |> printfn "%A"

[<aoc.Solution(2023, 8)>]
let day06 () =
    solve "data/2023/sample08.txt"
    solve "data/2023/input08.txt"

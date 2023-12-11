module aoc_2023_06

let split (c: char) (line: string) =
    line.Split(c, System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.toList

let parseLine line =
    line
    |> split ':'
    |> List.item 1
    |> split ' '
    |> List.map int64

let readInput =
    System.IO.File.ReadAllLines
    >> List.ofArray
    >> List.filter (System.String.IsNullOrWhiteSpace >> not)
    >> List.map parseLine
    >> function
    | [a; b] -> (a, b)
    | _ -> failwith "invalid line"
    >> fun (a, b) -> List.zip a b

let rec getSolution win timeLeft charge record =
    let current = timeLeft * charge
    if current > record then
        getSolution (win + 1) (timeLeft - 1L) (charge + 1L) record
    elif win = 0 then
        getSolution win (timeLeft - 1L) (charge + 1L) record
    else
        win

let solvePart1 config =
    config
    |> List.map (fun (time, distance) -> getSolution 0 time 0 distance)
    |> List.reduce (*)

let solvePart2 config =
    config
    |> List.reduce (fun (tA, dA) (tB, dB) ->
        (int64 (string tA + string tB), int64 (string dA + string dB)))
    |> fun (time, distance) ->
        getSolution 0 time 0 distance

let solve input =
    let config = readInput input
    config |> solvePart1 |> printfn "%A"
    config |> solvePart2 |> printfn "%A"

[<aoc.Solution(2023, 6)>]
let day06 () =
    solve "data/2023/sample06.txt"
    solve "data/2023/input06.txt"

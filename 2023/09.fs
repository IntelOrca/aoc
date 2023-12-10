module aoc_2023_09

let readInput =
    System.IO.File.ReadAllLines
    >> List.ofArray
    >> List.filter (System.String.IsNullOrWhiteSpace >> not)
    >> List.map (fun s -> s.Split(' ') |> List.ofArray |> List.map int)

let rec getDeltas acc numbers =
    match numbers with
    | head :: tail ->
        match tail with
        | neck :: _ -> getDeltas ((neck - head) :: acc) tail
        | _ -> acc |> List.rev
    | [] -> []

let rec getNext numbers =
    if List.exists ((<>) 0) numbers then
        let prev = numbers |> List.last
        let nextRowNext = numbers |> getDeltas [] |> getNext
        prev + nextRowNext
    else
        0

let solvePart1 = List.map getNext >> List.sum
let solvePart2 = List.map List.rev >> solvePart1

let solve input =
    let config = readInput input
    config |> solvePart1 |> printfn "%A"
    config |> solvePart2 |> printfn "%A"

[<aoc.Solution(2023, 9)>]
let day06 () =
    solve "data/2023/sample09.txt"
    solve "data/2023/input09.txt"

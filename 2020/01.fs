module aoc_2020_01

let readInput path =
    System.IO.File.ReadAllLines(path)
    |> Array.map (System.Int32.Parse)
    |> Array.toList

let toPairs list =
    list
    |> List.mapi (fun i a ->
        list
        |> List.mapi (fun j b -> (j, b))
        |> List.filter (fun (j, _) -> i < j)
        |> List.map (fun (_, b) -> (a, b)))
    |> List.collect id

let solvePart1 =
    toPairs
    >> List.map (fun (a, b) -> (a + b, a * b))
    >> List.find (fun (s, _) -> s = 2020)
    >> snd

let solve path =
    let config = readInput path
    config |> solvePart1 |> printfn "%d"

[<aoc.Solution(2020, 1)>]
let day01 () =
    solve "data/2020/sample01.txt"

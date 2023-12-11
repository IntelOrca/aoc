module aoc_2023_11

let readInput =
    System.IO.File.ReadAllLines
    >> List.ofArray
    >> List.filter (System.String.IsNullOrWhiteSpace >> not)
    >> List.map (fun s -> s.ToCharArray() |> List.ofArray)
    >> List.mapi (fun y row -> row |> List.mapi (fun x tile -> ((int64 x, int64 y), tile)))
    >> List.collect id
    >> List.filter (fun ((x, y), t) -> t = '#')
    >> List.map fst

let expand multiplier universe =
    let expandAxis f f2 universe =
        let size = universe |> List.map f |> List.max
        let empty =
            [0L..size - 1L]
            |> List.filter(fun i ->
                universe
                |> List.exists (fun pos -> f pos = i)
                |> not)
        let transform pos =
            let amount =
                empty
                |> List.filter (fun i -> i < f pos)
                |> List.length
                |> int64
            pos |> f2 (f pos + (amount * (multiplier - 1L)))
        universe |> List.map transform
    universe
    |> expandAxis fst (fun v (x, y) -> (v, y))
    |> expandAxis snd (fun v (x, y) -> (x, v))

let getPairs universe =
    universe
    |> List.mapi (fun i a ->
        universe
        |> List.mapi (fun j b -> (j, b))
        |> List.filter (fun (j, b) -> j > i)
        |> List.map snd
        |> List.map (fun b -> (a, b)))
    |> List.collect id

let getDistance ((xA, yA), (xB, yB)) =
    (abs (xB - xA)) + (abs (yB - yA))

let measureLengths universe =
    universe
    |> getPairs
    |> List.map getDistance
    |> List.sum

let solvePart multiplier = expand multiplier >> measureLengths
let solvePart1 = solvePart 2
let solvePart2 = solvePart 1000000

let solve input =
    let universe = readInput input
    universe |> solvePart1 |> printfn "%d"
    universe |> solvePart2 |> printfn "%d"

[<aoc.Solution(2023, 11)>]
let day06 () =
    solve "data/2023/sample11.txt"
    solve "data/2023/input11.txt"

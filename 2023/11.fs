module aoc_2023_11

let readInput =
    System.IO.File.ReadAllLines
    >> List.ofArray
    >> List.filter (System.String.IsNullOrWhiteSpace >> not)
    >> List.map (fun s -> s.ToCharArray() |> List.ofArray)
    >> List.mapi (fun y row -> row |> List.mapi (fun x tile -> ((x, y), tile)))
    >> List.collect id
    >> List.filter (fun ((x, y), t) -> t = '#')
    >> List.map fst

let expand universe =
    let expandAxis f f2 universe =
        let size = universe |> List.map f |> List.max
        let empty =
            [0..size - 1]
            |> List.filter(fun i ->
                universe
                |> List.exists (fun pos -> f pos = i)
                |> not)
        let transform pos =
            match empty |> List.tryFindIndex (fun i -> i > f pos) with
            | Some amount -> pos |> f2 (f pos + amount)
            | None -> pos
        universe |> List.map transform
    let setX = fun v (x, y) -> (v, y)
    let setY = fun v (x, y) -> (x, v)
    universe
    |> expandAxis fst setX
    |> expandAxis snd setY

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

let solvePart1 universe =
    universe
    |> expand
    |> measureLengths

let solvePart2 = solvePart1

let solve input =
    let universe = readInput input
    universe |> solvePart1 |> printfn "%A"
    universe |> solvePart2 |> printfn "%A"

[<aoc.Solution(2023, 11)>]
let day06 () =
    solve "data/2023/sample11.txt"
    // solve "data/2023/input11.txt"

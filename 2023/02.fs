module aoc_2023_02

open System
open System.Text.RegularExpressions

let regex pattern s =
    match (new Regex(pattern)).Match(s) with
    | null -> []
    | m ->
        m.Groups
        |> List.ofSeq
        |> List.tail
        |> List.map (fun g -> g.Value)

let parseSequence f (sep: char) (s: string) =
    s.Split(sep)
    |> List.ofArray
    |> List.map (fun s -> s.Trim())
    |> List.map f

let parseCubeGroup s =
    match parseSequence id ' ' s with
    | [count; colour] -> (int count, colour)
    | _ -> failwith "Invalid cube group"
let parseCubes s = parseSequence parseCubeGroup ',' s
let parseAttempts s = parseSequence parseCubes ';' s
let parseGame s =
    match s |> regex "Game (\d+): (.+)" with
    | [gameId; gameData] -> (int gameId, parseAttempts gameData)
    | _ -> failwith "Invalid game"

let readInput =
    System.IO.File.ReadAllLines
    >> List.ofArray
    >> List.filter (String.IsNullOrWhiteSpace >> not)
    >> List.map parseGame

let checkCubes bag cubes =
    let (count, colour) = cubes
    let total =
        bag
        |> List.tryFind (fun (_, col) -> col = colour)
        |> Option.map fst
        |> Option.defaultValue 0
    total >= count

let checkAttempt bag = List.forall (checkCubes bag)
let checkGame bag = snd >> List.forall (checkAttempt bag)
let checkGames bag =
    List.filter (checkGame bag)
    >> List.map fst
    >> List.sum

let getGamePower =
    snd
    >> List.collect id
    >> List.groupBy snd
    >> List.map (snd >> List.map fst >> List.max)
    >> List.reduce (*)
let getGamePowerSum = List.map getGamePower >> List.sum

[<aoc.Solution(2023, 2)>]
let day01 () =
    let bag = [(12, "red"); (13, "green"); (14, "blue")]
    let games = readInput "data/2023/sample02.txt"
    games |> checkGames bag |> printfn "Part A: %A"
    games |> getGamePowerSum |> printfn "Part B: %A"

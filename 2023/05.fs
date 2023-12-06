module aoc_2023_05

type Remap = {
    dst: int64
    src: int64
    len: int64
}

type Config = {
    seeds: int64 list
    maps: Remap list list
}

module String =
    let contains (c: char) (s: string) = s.Contains(c)
    let startsWith (value: string) (s: string) = s.StartsWith(value)
    let substring (index: int) (s: string) = s.Substring(index)

let parseNumbers (s: string) =
    s.Split(' ', System.StringSplitOptions.RemoveEmptyEntries)
    |> List.ofArray
    |> List.map int64

let rec parseBody acc lines =
    match lines with
    | head :: tail when not (head |> String.contains ':') ->
        parseBody (head :: acc) tail
    | _ -> (acc, lines)

let rec parse acc lines =
    match lines with
    | head :: tail when head |> String.startsWith "seeds:" ->
        let section = ("seeds:", [head |> String.substring 7 |> parseNumbers])
        parse (section :: acc) tail
    | head :: tail when head |> String.contains ':' ->
        let (body, remaining) = parseBody [] tail
        let section = (head, body |> List.map parseNumbers)
        parse (section :: acc) remaining
    | _ -> acc

let createMap input =
    input
    |> List.choose (fun input ->
        match input with
        | [dst; src; len] -> Some { dst = dst; src = src; len = len }
        | _ -> None)

let getConfig sections =
    let seeds =
        sections
        |> List.find (fun (heading, _) -> heading = "seeds:")
        |> snd
        |> List.head
    let maps =
        sections
        |> List.filter (fun (heading, _) -> heading <> "seeds:")
        |> List.map snd
        |> List.map createMap
    { seeds = seeds; maps = maps }

let readInput =
    System.IO.File.ReadAllLines
    >> List.ofArray
    >> List.filter (System.String.IsNullOrWhiteSpace >> not)
    >> parse []
    >> List.rev
    >> getConfig

let tryRemap input remap =
    if input >= remap.src && input < remap.src + remap.len then
        Some (remap.dst + (input - remap.src))
    else
        None

let getRemap input remap =
    match remap |> List.tryPick (tryRemap input) with
    | Some output -> output
    | None -> input

let getLocation config seed =
    let rec pipe rem input =
        match rem with
        | head :: tail ->
            pipe tail (head |> getRemap input)
        | _ -> input
    pipe config.maps seed

let solvePart1 config =
    config.seeds
    |> List.map (getLocation config)
    |> List.min

let solvePart2 config =
    let ranges =
        config.seeds
        |> List.chunkBySize 2
    let mutable result = System.Int64.MaxValue
    for range in ranges do
        let [rangeStart: int64; len] = range
        let rangeEnd = rangeStart + len
        let mutable i = rangeStart
        while i < rangeEnd do
            let l = getLocation config i
            if l < result then
                result <- l
    result

[<aoc.Solution(2023, 5)>]
let day05 () =
    let config = readInput "data/2023/input05.txt"
    config |> solvePart1 |> printfn "%d"
    config |> solvePart2 |> printfn "%d"

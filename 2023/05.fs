module aoc_2023_05

type Remap = {
    dst: int64
    src: int64
    len: int64
}

type Transformer = int64 -> int64
type Config = {
    seeds: int64 list
    transformer: Transformer
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

let createSubTransformer input =
    match input with
    | [dst; src; len] ->
        fun i ->
            if i >= src && i < src + len then
                Some (dst + (i - src))
            else
                None
    | _ -> fun _ -> None

let createTransformer input =
    let subTransformers =
        input
        |> List.map createSubTransformer
        |> List.append [fun i -> Some i]
    fun i ->
        subTransformers
        |> List.pick (fun t -> t i)

let createRootTransformer input =
    fun i ->
        input
        |> List.map createTransformer
        |> List.fold (fun j f -> f j) i

let getConfig sections =
    let seeds =
        sections
        |> List.find (fun (heading, _) -> heading = "seeds:")
        |> snd
        |> List.head
    let transformer =
        sections
        |> List.filter (fun (heading, _) -> heading <> "seeds:")
        |> List.map snd
        |> createRootTransformer
    { seeds = seeds; transformer = transformer }

let readInput =
    System.IO.File.ReadAllLines
    >> List.ofArray
    >> List.filter (System.String.IsNullOrWhiteSpace >> not)
    >> parse []
    >> List.rev
    >> getConfig

let solvePart1 config =
    config.seeds
    |> List.map config.transformer
    |> List.min

let solvePart2 config =
    solvePart1 config

[<aoc.Solution(2023, 5)>]
let day05 () =
    let config = readInput "data/2023/sample05.txt"
    config |> solvePart1 |> printfn "%d"
    config |> solvePart2 |> printfn "%d"

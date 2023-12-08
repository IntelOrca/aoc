module aoc_2023_05

type Range = int64 * int64
type Transformer = Range list -> Range list
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

let createRange s e = (s, e - s)
let intersectRange (tStart, tLen) (inStart, inLen) =
    let tEnd = tStart + tLen
    let inEnd = inStart + inLen
    if inEnd <= tStart || inStart >= tEnd then
        ([createRange inStart inEnd], [])
    elif inStart < tStart && inEnd > tStart && inEnd <= tEnd then
        ([createRange inStart tStart], [createRange tStart tEnd])
    elif inStart >= tStart && inStart < tEnd && inEnd > tEnd then
        ([createRange tEnd inEnd], [createRange inStart tEnd])
    elif inStart < tStart && inEnd > tEnd then
        ([createRange inStart tStart; createRange tEnd inEnd], [createRange tStart tEnd])
    else
        ([], [createRange inStart inEnd])

let createSubTransformer input =
    match input with
    | [dst; src; len] ->
        let transformRange (rStart, rLen) = (dst + (rStart - src), rLen)
        fun ranges ->
            match ranges with
            | [] -> ([], [])
            | _ ->
                let (unmapped, mapped) =
                    ranges
                    |> List.map (intersectRange (src, len))
                    |> List.reduce (fun (u0, m0) (u1, m1) ->
                        (u0 @ u1, m0 @ m1))
                let transformed =
                    mapped
                    |> List.map transformRange
                (unmapped, transformed)
    | _ -> failwith "Invalid input for transformer"

let createTransformer input =
    let rec foldTransformers mapped unmapped transformers =
        match transformers with
        | head :: tail ->
            let (unmapped2, mapped2) = head unmapped
            foldTransformers (mapped @ mapped2) unmapped2 tail
        | [] ->
            mapped @ unmapped

    let transformers =
        input
        |> List.map createSubTransformer

    fun unmapped -> foldTransformers [] unmapped transformers

let createRootTransformer input =
    fun unmapped ->
        input
        |> List.map createTransformer
        |> List.fold (fun unmapped f -> f unmapped) unmapped

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
    |> List.map (fun i -> [(i, 1L)])
    |> List.collect config.transformer
    |> List.map fst
    |> List.min

let solvePart2 config =
    config.seeds
    |> List.chunkBySize 2
    |> List.map (function
        | [a; b] -> [(a, b)]
        | _ -> [])
    |> List.collect config.transformer
    |> List.map fst
    |> List.min

let solve input =
    let config = readInput input
    config |> solvePart1 |> printfn "%d"
    config |> solvePart2 |> printfn "%d"

[<aoc.Solution(2023, 5)>]
let day05 () =
    solve "data/2023/sample05.txt"
    solve "data/2023/input05.txt"

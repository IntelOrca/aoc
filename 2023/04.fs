module aoc_2023_04

type card = {
    number: int
    winners: int list
    matches: int list
}

let split (sep: char) (line: string) =
    line.Split(sep, System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun x -> x.Trim())
    |> Array.toList

let parseNumbers s =
    s |> split ' ' |> List.map int

let parseLine index line =
    let sides =
        line
        |> split ':'
        |> List.item 1
        |> split '|'
    match sides with
    | [winners; matches] ->
        { number = index + 1
          winners = parseNumbers winners
          matches = parseNumbers matches }
    | _ -> failwith "invalid game"

let readInput =
    System.IO.File.ReadAllLines
    >> List.ofArray
    >> List.filter (System.String.IsNullOrWhiteSpace >> not)
    >> List.mapi parseLine

let getNumWinners card =
    let a = card.matches |> set
    let b = card.winners |> set
    Set.intersect a b |> Set.count

let getScore card =
    let count = getNumWinners card
    if count = 0 then 0
    else 1 <<< (count - 1)

let findCard n cards =
    cards |> List.tryFind (fun x -> x.number = n)

let takeSimilar cards =
    let rec takeSimilarInner acc cards =
        let last = List.head acc
        match cards with
        | head :: tail when head.number = last.number ->
            takeSimilarInner (head :: acc) tail
        | _ -> (acc, cards)
    match cards with
    | head :: tail -> takeSimilarInner [head] tail
    | [] -> ([], [])

module List =
    let multiply multiplier lst =
        (fun _ -> lst)
        |> List.init multiplier
        |> List.collect id

let rec processGames score cards =
    let (similar, remaining) = takeSimilar cards
    match similar with
    | [] -> score
    | head :: _ ->
        let multiplier = List.length similar
        let count = getNumWinners head
        let newCards =
            List.init count (fun i -> head.number + i + 1)
            |> List.choose (fun n -> findCard n cards)
            |> List.multiply multiplier
            |> List.append remaining
            |> List.sortBy (fun c -> c.number)
        processGames (score + multiplier) newCards

let solvePart1 = List.map getScore >> List.sum
let solvePart2 cards = processGames 0 cards

[<aoc.Solution(2023, 4)>]
let day04 () =
    let cards = readInput "data/2023/sample04.txt"
    cards |> solvePart1 |> printfn "%A"
    cards |> solvePart2 |> printfn "%A"

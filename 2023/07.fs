module aoc_2023_07

let parseCard = function
    | d when d >= '2' && d <= '9' -> int d - int '0'
    | 'T' -> 10
    | 'J' -> 11
    | 'Q' -> 12
    | 'K' -> 13
    | 'A' -> 14
    | _ -> failwith "invalid card"

let parseHand (hand: string) =
    hand.ToCharArray()
    |> List.ofArray
    |> List.map parseCard

let parseHandBid (line: string) =
    match line.Split(' ') with
    | [|a; b|] -> (parseHand a, int b)
    | _ -> failwith "invalid line"

let readInput =
    System.IO.File.ReadAllLines
    >> List.ofArray
    >> List.filter (System.String.IsNullOrWhiteSpace >> not)
    >> List.map parseHandBid

let isJoker = (=) 0
let getGroupCounts hand =
    let hand =
        match hand |> List.filter isJoker |> List.length with
        | 0 -> hand
        | _ ->
            let cardToClone =
                hand
                |> List.filter (isJoker >> not)
                |> List.groupBy id
                |> List.sortByDescending (fun (k, v) -> (List.length v, k))
                |> List.map fst
                |> List.tryHead
                |> Option.defaultValue 14
            hand
            |> List.map (fun i -> if isJoker i then cardToClone else i)
    hand
    |> List.groupBy id
    |> List.map (snd >> List.length)
    |> List.sortByDescending id

let getHand hand = (hand |> getGroupCounts, hand)

let solvePart1 =
    List.sortBy (fst >> getHand)
    >> List.mapi (fun i (_, bid) -> (i + 1) * bid)
    >> List.sum

let solvePart2 config =
    let replaceWithJoker i = if i = 11 then 0 else i
    let replaceWithJokers = List.map replaceWithJoker
    config
    |> List.map (fun (a, b) -> (a |> replaceWithJokers, b))
    |> solvePart1

let solve input =
    let config = readInput input
    config |> solvePart1 |> printfn "%d"
    config |> solvePart2 |> printfn "%d"

[<aoc.Solution(2023, 7)>]
let day06 () =
    solve "data/2023/sample07.txt"
    solve "data/2023/input07.txt"

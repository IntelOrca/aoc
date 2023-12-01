module aoc_2023_01

type Part = A | B
let part = A

let toChars (s: string) = s.ToCharArray() |> Array.toList

let readInput path =
    System.IO.File.ReadAllLines(path)
    |> Array.filter (System.String.IsNullOrWhiteSpace >> not)
    |> Array.map toChars
    |> Array.toList

let numericDigits =
    [0..9]
    |> List.map (fun x -> (x |> string |> toChars, x))
let wordDigits =
    [ "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine" ]
    |> List.map toChars
    |> List.mapi (fun i s -> (s, i + 1))

let tokenList =
    match part with
    | A -> numericDigits
    | B -> numericDigits @ wordDigits

let parseToken token s =
    let head = s |> List.truncate (List.length token)
    head = token

let parse tokens s =
    tokens
    |> List.tryFind (fun (t, i) -> parseToken t s)
    |> Option.map snd

let rec findToken tokens s =
    match parse tokens s with
    | Some i -> Some i
    | None ->
        match s with
        | _ :: tail -> tail |> findToken tokens
        | [] -> None

let findTokenFront = findToken tokenList
let findTokenBack s =
    let revTokenList =
        tokenList
        |> List.map (fun (s, i) -> (List.rev s, i))
    s
    |> List.rev
    |> findToken revTokenList

let convertString (s: char list) =
    let first = findTokenFront s |> Option.get |> string
    let last = findTokenBack s |> Option.get |> string
    first + last |> int

let inputPath =
    match part with
    | A -> 'a'
    | B -> 'b'
    |> sprintf "data/2023/sample01.%c.txt"

[<aoc.Solution(2023, 1)>]
let day01 () =
    inputPath
    |> readInput
    |> List.map convertString
    |> List.sum
    |> printfn "%d"

module aoc_2023_03

type Position = (int * int)
type Part = Position * int

type EngineThing =
    | Annotation of Position
    | Gear of Position
    | Part of Part

type ParseState = {
    x: int
    y: int
    line: char list
}

let (|Digit|_|) state =
    match state.line with
    | head :: tail when head >= '0' && head <= '9' ->
        let next = { state with line = tail; x = state.x + 1 }
        Some (next, int head - int '0')
    | _ -> None

let (|Number|_|) state =
    match state with
    | Digit _ ->
        let rec parseNumber num state =
            match state with
            | Digit (state, d) -> parseNumber (num * 10 + d) state
            | _ -> (state, num)
        let (next, number) = parseNumber 0 state
        Some (next, Part ((state.x, state.y), number))
    | _ -> None

let (|Symbol|_|) state =
    match state.line with
    | head :: tail ->
        let next = { state with line = tail; x = state.x + 1 }
        match head with
        | '*' -> Some (next, Gear (state.x, state.y))
        | '.' -> None
        | _ -> Some (next, Annotation (state.x, state.y))
    | _ -> None

let (|Blank|_|) state =
    match state.line with
    | head :: tail when head = '.' ->
        Some { state with line = tail; x = state.x + 1 }
    | _ -> None

let rec parseLine things state =
    match state with
    | Number (state, t) -> parseLine (t :: things) state
    | Symbol (state, t) -> parseLine (t :: things) state
    | Blank state -> parseLine things state
    | _ -> things

let readInput =
    System.IO.File.ReadAllLines
    >> List.ofArray
    >> List.map (fun line -> line.ToCharArray() |> Array.toList)
    >> List.mapi (fun y line -> parseLine [] { x = 0; y = y; line = line })
    >> List.collect id
    >> List.sortBy (function
        | Annotation (x, y) -> (y, x)
        | Gear (x, y) -> (y, x)
        | Part ((x, y), _) -> (y, x))

let numLen = abs >> (+) 0.1 >> log10 >> ceil >> int
let getBorder (x, y) =
    [(-1, -1); (0, -1); (1, -1);
     (-1,  0);          (1,  0);
     (-1,  1); (0,  1); (1,  1)]
    |> List.map (fun (dx, dy) -> (x + dx, y + dy))
let getBorderN ((x, y): Position, num: int) =
    [0..(numLen num - 1)]
    |> List.map (fun i -> (x + i, y))
    |> List.collect getBorder
    |> List.distinct

let createCheckFn things: Part -> bool =
    let triggerPositions =
        things
        |> List.choose (function
            | Annotation pos -> Some pos
            | Gear pos -> Some pos
            | _ -> None)
        |> set
    let checkPosition = fun x -> Set.contains x triggerPositions
    getBorderN >> List.exists checkPosition

let getImportantPart checkFn = function
    | Part p when checkFn p -> Some p
    | _ -> None

let getGear = function
    | Gear a -> Some a
    | _ -> None

let getGearRatio things gear =
    let checkPosition = createCheckFn [gear]
    let products =
        things
        |> List.choose (getImportantPart checkPosition)
        |> List.map snd
    match products with
    | [] -> 0
    | [_] -> 0
    | items -> items |> List.reduce (*)

let solvePart1 things =
    let checkPosition = createCheckFn things
    things
    |> List.choose (getImportantPart checkPosition)
    |> List.map snd
    |> List.sum

let solvePart2 things =
    things
    |> List.filter (getGear >> Option.isSome)
    |> List.map (getGearRatio things)
    |> List.sum

[<aoc.Solution(2023, 3)>]
let day03 () =
    let things = readInput "data/2023/sample03.txt"
    things |> solvePart1 |> printfn "%A"
    things |> solvePart2 |> printfn "%A"

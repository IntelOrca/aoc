module aoc_2023_10

type Direction = North | East | South | West
type TileKind =
    | NorthSouth
    | WestEast
    | NorthEast
    | NorthWest
    | SouthWest
    | SouthEast
    | Empty
    | Start
type Position = int * int
type Tile = Position * TileKind

let getType = function
    | '|' -> NorthSouth
    | '-' -> WestEast
    | 'L' -> NorthEast
    | 'J' -> NorthWest
    | '7' -> SouthWest
    | 'F' -> SouthEast
    | 'S' -> Start
    | _ -> Empty

let readInput =
    System.IO.File.ReadAllLines
    >> List.ofArray
    >> List.filter (System.String.IsNullOrWhiteSpace >> not)
    >> List.map (fun s -> s.ToCharArray() |> List.ofArray |> List.map getType)
    >> List.mapi (fun y row -> row |> List.mapi (fun x tile -> ((x, y), ((x, y), tile))))
    >> List.collect id
    >> Map.ofList

let getTile pos map = map |> Map.tryFind pos

let getNeighbour map dir ((x, y), _) =
    match dir with
    | North -> map |> getTile (x, y - 1)
    | East -> map |> getTile (x + 1, y)
    | South -> map |> getTile (x, y + 1)
    | West -> map |> getTile (x - 1, y)

let isConnected dir (_, kind) =
    match kind with
    | NorthSouth -> dir = North || dir = South
    | WestEast -> dir = West || dir = East
    | NorthEast -> dir = South || dir = West
    | NorthWest -> dir = South || dir = East
    | SouthWest -> dir = North || dir = East
    | SouthEast -> dir = North || dir = West
    | Start -> true
    | _ -> false

let getConnectedNeighbour map dir tile =
    match getNeighbour map dir tile with
    | Some neighbour when neighbour |> isConnected dir -> Some neighbour
    | _ -> None

let getAvailableDirections (_, kind) =
    match kind with
    | NorthSouth -> [North; South]
    | WestEast -> [West; East]
    | NorthEast -> [North; East]
    | NorthWest -> [North; West]
    | SouthWest -> [South; West]
    | SouthEast -> [South; East]
    | Start -> [North; South; West; East]
    | _ -> []

let findConnectedTiles map tile =
    tile
    |> getAvailableDirections
    |> List.choose (fun dir -> getConnectedNeighbour map dir tile)

let findConnectedTile map tile prev =
    findConnectedTiles map tile
    |> List.filter (fun t -> Some t <> prev)
    |> List.tryHead

let getLoop map =
    let start = map |> Map.values |> Seq.find (fun (_, kind) -> kind = Start)
    let rec findNext acc tile prev =
        match findConnectedTile map tile prev with
        | Some neighbour when neighbour = start -> acc
        | Some neighbour -> findNext (neighbour :: acc) neighbour (Some tile)
        | None -> failwith "No loop found"
    let result = findNext [] start None
    let newStart =
        let neighbours = findConnectedTiles map start
        [NorthSouth; WestEast; NorthEast; NorthWest; SouthWest; SouthEast]
        |> List.map (fun kind -> (start |> fst, kind))
        |> List.find (fun t -> findConnectedTiles map t = neighbours)
    newStart :: result

let solvePart1 =
    getLoop
    >> List.length
    >> fun l -> l / 2

let solvePart2 map =
    let map =
        map
        |> getLoop
        |> List.map (fun (pos, kind) -> (pos, (pos, kind)))
        |> Map.ofList

    let maxX = map |> Map.keys |> Seq.map fst |> Seq.max
    let rec countEnclosed (x, y) count state =
        if x > maxX then count
        else
            let next = countEnclosed (x + 1, y)
            let kind =
                map
                |> getTile (x, y)
                |> Option.map snd
            match kind with
            | Some NorthSouth -> next count (state ^^^ 1)
            | Some WestEast -> next count state
            | Some NorthEast -> next count (state ^^^ 1)
            | Some NorthWest -> next count (state ^^^ 1)
            | Some SouthWest -> next count state
            | Some SouthEast -> next count state
            | Some Start -> failwith "not implemented"
            | _ -> next (count + state) state
    map
    |> Map.keys
    |> List.ofSeq
    |> List.map snd
    |> List.distinct
    |> List.sort
    |> List.map (fun y -> countEnclosed (0, y) 0 0)
    |> List.sum

let solve input =
    let config = readInput input
    config |> solvePart1 |> printfn "%A"
    config |> solvePart2 |> printfn "%A"

[<aoc.Solution(2023, 10)>]
let day06 () =
    solve "data/2023/sample10.txt"
    solve "data/2023/input10.txt"

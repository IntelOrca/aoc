module aoc_2022_08

type Tree = { position: (int * int); height: int }

let parseInt i = (int i) - (int '0')
let readInput path =
    System.IO.File.ReadAllLines(path)
    |> Array.map (fun x -> x.ToCharArray())
    |> Array.mapi (fun row cols ->
        cols
        |> Array.mapi (fun col h -> { position = (col, row); height = parseInt h }))
    |> Array.collect id
    |> Array.toList

let getTreeHeight forest pos =
    forest
    |> List.tryFind (fun t -> t.position = pos)
    |> Option.map (fun t -> t.height)

let rec isUnobscured forest tree (fx, fy) =
    let rec isUnobscuredInner (x, y) =
        let nextPos = (fx x 1, fy y 1)
        let next = getTreeHeight forest nextPos
        match next with
        | Some h ->
            if h >= tree.height then false
            else isUnobscuredInner nextPos
        | None -> true
    isUnobscuredInner tree.position

let isVisible forest tree =
    [((-), (/)); ((+), (/));
     ((/), (-)); ((/), (+))]
    |> List.exists (isUnobscured forest tree)

let solvePart1 forest =
    forest
    |> List.filter (isVisible forest)
    |> List.length

let solve path =
    let forest = readInput path
    forest |> solvePart1 |> printfn "%d"

[<aoc.Solution(2022, 8)>]
let day08 () =
    solve "data/2022/sample08.txt"

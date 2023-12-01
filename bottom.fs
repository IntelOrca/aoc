[<EntryPoint>]
let main argv = 
    match argv |> Array.toList with
    | year :: day :: tail ->
        let y = int year
        let d = int day
        match aoc.getSolution y d with
        | Some problem ->
            printfn "https://adventofcode.com/%d/day/%d:" y d
            problem.solution ()
            0
        | None ->
            printfn "No solution found for %d/%d" y d
            1
    | _ ->
        printfn "%s" "usage: <year> <day>"
        1

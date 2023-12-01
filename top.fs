module aoc

open System
open System.Reflection

[<AttributeUsage(AttributeTargets.Method, AllowMultiple = false)>]
type SolutionAttribute(year: int, day: int) =
    inherit Attribute()
    member this.Year = year
    member this.Day = day

type Problem = {
    year: int
    day: int
    solution: unit -> unit
}

let problems: Problem list =
    let allTypes =
        let rec harvestTypes (t: Type): Type list =
            let nestedTypes = t.GetNestedTypes() |> Array.toList
            let descendants =
                nestedTypes
                |> List.collect harvestTypes
                |> List.append nestedTypes
            t :: descendants
        Assembly.GetExecutingAssembly().GetTypes()
        |> List.ofArray
        |> List.collect harvestTypes
    allTypes
    |> Seq.collect (fun typ -> typ.GetMethods())
    |> Seq.filter (fun methodInfo -> 
        methodInfo.GetCustomAttributes(typeof<SolutionAttribute>, true).Length > 0)
    |> Seq.map (fun methodInfo ->
        let solutionAttribute = 
            methodInfo.GetCustomAttributes(typeof<SolutionAttribute>, true)
            |> Seq.cast<SolutionAttribute>
            |> Seq.head
        let solution () =
            methodInfo.Invoke(null, [||]) |> ignore
        { solution = solution; year = solutionAttribute.Year; day = solutionAttribute.Day })
    |> Seq.toList

let getSolution year day =
    problems
    |> List.tryFind (fun p -> p.year = year && p.day = day)

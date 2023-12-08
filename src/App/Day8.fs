module Day8

open FParsec

let pname = manyChars (satisfy isAsciiUpper)
let ptuple = pchar '(' >>. pname .>> pstring ", " .>>. pname .>> pchar ')'
let pnode = pname .>> pstring " = " .>>. ptuple

type Graph = Map<string, (string * string)>
module Graph =
    let ofNodes nodes =
        let folder acc (src, dest) = Map.add src dest acc
        List.fold folder Map.empty nodes

let getNext instr graph current =
    let (left, right) = Map.find current graph
    if instr = 'L' then left else right

let computeStepsToDestination pred node instructions graph =
    let rec run current instrs steps =
        match current, instrs with
        | c , _ when pred c -> steps
        | _, [] -> run current instructions steps
        | c, x::xs -> run (getNext x graph c) xs (steps + 1)
    run node instructions 0

let endsWith (c: char) (s: string) = s.EndsWith(c)

let rec gcd a b =
    if b = 0UL then a
    else gcd b (a % b)

let lcm a b = a * (b / (gcd a b))

let part2 instructions graph =
    let nodes = Seq.filter (endsWith 'A') (Map.keys graph) |> List.ofSeq
    let computeDistance node = computeStepsToDestination (endsWith 'Z') node instructions graph
    let distances = List.map (computeDistance >> uint64) nodes
    let shortestDistance = List.reduce lcm distances
    printfn "part 2: %d" shortestDistance


let run lines =
    let instructions = List.head lines |> List.ofSeq
    let graph = List.map (Utils.Parsing.runf pnode) (List.skip 2 lines) |> Graph.ofNodes
    let part1 = computeStepsToDestination ((=) "ZZZ") "AAA" instructions graph
    printfn "part 1: %d" part1
    part2 instructions graph

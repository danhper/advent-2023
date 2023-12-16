module Day16

open Utils

type Cell = Empty | RMirror | LMirror  | VSplitter | HSplitter
module Cell =
    let fromChar = function
    | '.' -> Empty
    | '/' -> RMirror
    | '\\' -> LMirror
    | '|' -> VSplitter
    | '-' -> HSplitter
    | c -> failwithf "invalid cell %c" c

let computeNextBeams position dir { map = map } =
    let next d = (Direction.moveTo position d, d)
    let nextBeams =
        match Map.find position map, dir with
        (* no changes *)
        | Empty, _
        | VSplitter, South | VSplitter, North
        | HSplitter, West | HSplitter, East -> [next dir]

        (* direction change *)
        | RMirror, South | LMirror, North -> [next West]
        | RMirror, North | LMirror, South -> [next East]
        | RMirror, East | LMirror, West -> [next North]
        | RMirror, West | LMirror, East -> [next South]

        (* split *)
        | VSplitter, _ -> [next South; next North]
        | HSplitter, _ -> [next East; next West]
    List.filter (fst >> flip Map.containsKey map) nextBeams

let findEnergizedCells init grid =
    let rec run beams seen =
        if Set.isEmpty beams then seen |> Set.map fst
        else
            let nextSeen = Set.union seen beams
            let getNextBeams (point, dir) = Set.ofList (computeNextBeams point dir grid)
            let nextBeams = Set.unionMany (List.map getNextBeams (Set.toList beams))
            run (Set.difference nextBeams seen) nextSeen
    run (Set.ofList [init]) Set.empty

let findMaxEnergizedCells ({ height = height; width = width } as grid) =
    let leftStart = List.map (fun y -> ((0, y), East)) [0..height - 1]
    let rightStart = List.map (fun y -> ((width - 1, y), West)) [0..height - 1]
    let topStart = List.map (fun x -> ((x, 0), South)) [0..width - 1]
    let bottomStart = List.map (fun x -> ((x, height - 1), North)) [0..width - 1]
    let allStarts = List.concat [leftStart; rightStart; topStart; bottomStart]
    let energizedCounts = Array.Parallel.map ((flip findEnergizedCells) grid >> Set.count) (Array.ofList allStarts)
    Array.max energizedCounts

let run lines =
    let grid = Grid2D.fromLines lines |> Grid2D.map (fun _ c -> Cell.fromChar c)
    let energizedCells = findEnergizedCells ((0, 0), East) grid
    printfn "part 1: %d" (Set.count energizedCells)
    printfn "part 2: %d" (findMaxEnergizedCells grid)

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

let computeNextBeams (position, dir) { map = map } =
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

let computeEnergizedCellsCount grid init =
    let rec run beams seen =
        match beams with
        | [] -> Set.count <| Set.map fst seen
        | b :: bs when Set.contains b seen -> run bs seen
        | b :: bs -> run (computeNextBeams b grid @ bs) (Set.add b seen)
    run [init] Set.empty

let computeMaxEnergizedCellsCount ({ height = height; width = width } as grid) =
    let yStarts = List.collect (fun y -> [((0L, y), East); ((width - 1L, y), West)]) [0L..height - 1L]
    let xStarts = List.collect (fun x -> [((x, 0L), South); ((x, height - 1L), North)]) [0L..width - 1L]
    let energizedCounts = Array.Parallel.map (computeEnergizedCellsCount grid) (Array.ofList (yStarts @ xStarts))
    Array.max energizedCounts

let run lines =
    let grid = Grid2D.fromLines lines |> Grid2D.map (fun _ c -> Cell.fromChar c)
    printfn "part 1: %d" (computeEnergizedCellsCount grid ((0, 0), East))
    printfn "part 2: %d" (computeMaxEnergizedCellsCount grid)

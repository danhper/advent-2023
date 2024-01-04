module Day14

open Utils

type Direction = North | West | East | South

let roll direction grid =
    let (sortBy, isDone, getNext) =
        match direction with
        | North -> snd, (fun (_, y) -> y = 0L), (fun (x, y) -> (x, y - 1L))
        | West -> fst, (fun (x, _) -> x = 0L), (fun (x, y) -> (x - 1L, y))
        | South -> snd >> (~-), (fun (_, y) -> y = grid.height - 1L), (fun (x, y) -> (x, y + 1L))
        | East -> fst >> (~-), (fun (x, _) -> x = grid.width - 1L), (fun (x, y) -> (x + 1L, y))

    let (round, newMap) = Map.partition (fun _ v -> v = 'O') grid.map
    let keys = round |> Map.keys |> Seq.toList
    let orderedKeys = List.sortBy sortBy keys

    let rec findPosition m p =
        if isDone p then p
        else
            let next = getNext p
            match Map.tryFind next m with
            | Some '#' | Some 'O' -> p
            | _ -> findPosition m next

    let folder m p = Map.add (findPosition m p) 'O' m
    { grid with map = List.fold folder newMap orderedKeys }

let getRoundPositions { map = map } = map |> Map.filter (fun _ v -> v = 'O') |> Map.keys

let computeLoad ({ height = height } as grid) =
    Seq.sumBy (fun (_, y) -> height - y) (getRoundPositions grid)

let part2 grid =
    let fullCycle = [North; West; South; East]
    let rec findRepetition i cycle currentGrid seen =
        let positions = Set.ofSeq (getRoundPositions currentGrid)
        match Map.tryFind positions seen, cycle with
        | Some i', _ -> (i', i, currentGrid)
        | _, [] -> findRepetition i fullCycle currentGrid seen
        | _, c :: cs ->
            findRepetition (i + 1) cs (roll c currentGrid) (Map.add positions i seen)

    let rec runCycles i cycle currentGrid =
        match i, cycle with
        | 0, _ -> currentGrid
        | _, [] -> runCycles i fullCycle currentGrid
        | _, c :: cs -> runCycles (i - 1) cs (roll c currentGrid)

    let (start, finish, newGrid) = findRepetition 0 fullCycle grid Map.empty
    let runsLeft = int ((4_000_000_000UL - uint64 start) % (uint64 finish - uint64 start))
    let finalGrid = runCycles runsLeft (List.skip (finish % 4) fullCycle) newGrid
    printfn "part 2: %d" (computeLoad finalGrid)

let run lines =
    let grid = Grid2D.fromLines lines
    let newGrid = roll North grid
    printfn "part 1: %d" (computeLoad newGrid)
    part2 grid

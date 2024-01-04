module Day23

open FSharpx.Collections
open Utils

// paths (.), forest (#), and steep slopes (^, >, v, and <).

type Cell = Path | Forest | Slope of Direction
module Cell =
    let ofChar = function
    | '.' -> Path
    | '#' -> Forest
    | '^' -> Slope North
    | 'v' -> Slope South
    | '<' -> Slope West
    | '>' -> Slope East
    | c -> failwithf "invalid char %c" c

    let ofCharNoSlopes = function
    | '#' -> Forest
    | _ -> Path

let getNextPositions position grid =
    let neighbors = Grid2D.neighbors false position grid
    let transformNeighbor = function
    | p, Path -> Some p
    | p, Slope d -> Some (Direction.moveTo p d)
    | _, Forest -> None
    List.choose transformNeighbor neighbors

let constructGraph grid =
    let final = (grid.width - 2L, grid.height - 1L)

    let rec getNextNode distance previous current =
        let candidates = getNextPositions current grid
        let newDistance = distance + Point2D.manhattanDistance previous current
        match candidates with
        | [] -> None
        | _ when current = final -> Some (current, newDistance)
        | [next] -> getNextNode newDistance current next
        | [p; next] when p = previous -> getNextNode newDistance current next
        | [next; p] when p = previous -> getNextNode newDistance current next
        | _ -> Some (current, newDistance)

    let rec run queue graph =
        match Queue.tryUncons queue with
        | None -> graph
        | Some (current, rest) when current = final || Map.containsKey current graph -> run rest graph
        | Some (current, rest) ->
            let nextCandidates = getNextPositions current grid
            let nextNodes = List.choose (getNextNode 0 current) nextCandidates
            let folder (g, q) ((point, _) as edge) =
                (Map.change current (prependToOption edge) g, Queue.conj point q)
            let (newGraph, newQueue) = List.fold folder (graph, rest) nextNodes
            run newQueue newGraph

    run (Queue.ofList [(1L, 0L)]) Map.empty

let findPaths grid =
    let start = (1L, 0L)
    let final = (grid.width - 2L, grid.height - 1L)
    let graph = constructGraph grid

    let rec run current distance seen =
        if current = final then [distance]
        else
            let nextSeen = Set.add current seen
            let isNew (p, _) = not (Set.contains p nextSeen)
            let nextCandidates = List.filter isNew (Map.find current graph)
            let mapper (p, d) = run p (distance + d) nextSeen
            List.collect mapper nextCandidates

    run start 0 Set.empty

let run lines =
    let grid = Grid2D.fromLines lines |> Grid2D.map (fun _ c -> Cell.ofChar c)
    let paths = findPaths grid
    printfn "part 1: %d" (List.max paths)

    let gridNoSlopes = Grid2D.fromLines lines |> Grid2D.map (fun _ c -> Cell.ofCharNoSlopes c)
    let pathsNoSlopes = findPaths gridNoSlopes
    printfn "part 2: %d" (List.max pathsNoSlopes)

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
    
    let toChar = function
    | Path -> '.' 
    | Forest -> '#' 
    | Slope North -> '^' 
    | Slope South -> 'v'
    | Slope West -> '<' 
    | Slope East -> '>' 

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

let findPaths grid =
    let start = (1, 0)
    let final = (grid.width - 2, grid.height - 2)

    let maxDistances = ref Map.empty
    let rec run current distance distances seen =
        if current = final then
            let decideWith _ v1 v2 = max v1 v2
            // maxDistances.Value <- Map.unionWith decideWith distances maxDistances.Value
            [distance]
        else
            match Map.tryFind current maxDistances.Value with
            | Some d when d > distance -> [d]
            | _ ->
                // maxDistances.Value <- Map.add current distance maxDistances.Value
                let nextCandidates = getNextPositions current grid
                let nextSeen = Set.add current seen
                let nextPositions = List.filter (fun p -> not (Set.contains p nextSeen)) nextCandidates
                let mapper p =
                    let nextDistance = distance + Point2D.manhattanDistance current p
                    let nextDistances = Map.add p nextDistance distances
                    run p nextDistance nextDistances nextSeen
                List.collect mapper nextPositions

    let result = run start 1 (Map.ofList [(start, 1)]) Set.empty
    // let gridFoo = Grid2D.map (fun p v -> Map.tryFind p maxDistances.Value |> Option.defaultValue -1 |> sprintf "%c(%4d)" (Cell.toChar v)) grid
    // printfn "%s" (Grid2D.toString " " gridFoo)
    result

let run lines =
    let grid = Grid2D.fromLines lines |> Grid2D.map (fun _ c -> Cell.ofChar c)
    let paths = findPaths grid
    printfn "part 1: %d" (List.max paths)

    let gridNoSlopes = Grid2D.fromLines lines |> Grid2D.map (fun _ c -> Cell.ofCharNoSlopes c)
    let pathsNoSlopes = findPaths gridNoSlopes
    printfn "part 2: %d" (List.max pathsNoSlopes)

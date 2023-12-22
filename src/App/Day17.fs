module Day17

open Utils
open FSharpx.Collections

let getNextPoints point dir count =
    let nextPossibility d = (Direction.moveTo point d, d, 2)
    let nextPoints = List.filter (fun d -> d <> dir && d <> (Direction.inv dir)) Direction.all |> List.map nextPossibility
    if count = 0 then nextPoints
    else (Direction.moveTo point dir, dir, count - 1) :: nextPoints


let outputDistances { map = map; height = height; width = width } distances =
    let mapper p _ = Map.tryFind p distances |> Option.map (fun v -> sprintf "%3d" v) |> Option.defaultValue "XX"
    let disMap = Map.map mapper map
    let disGrid = { map = disMap; height = height; width = width }
    printfn "%s" (Grid2D.toString "XX" disGrid)

let traverse ({ map = map; height = height; width = width } as grid) =
    outputDistances grid map
    printfn ""
    let endPoint = (width - 1, height - 1)
    let rec run queue distances seen =
        if Set.isEmpty queue then failwith "could not find path"
        else
            let ((distance, current, dir, count) as elem) = Set.minElement queue
            if current = endPoint then
                outputDistances grid distances
                distance
            else
                let pred (p, _, _) = Map.containsKey p map && not (Set.contains p seen)
                let nextPoints = getNextPoints current dir count |> List.filter pred
                let mapper (p, d, c) =
                    // let currentDistance = Map.tryFind p distances |> Option.defaultValue 99999999
                    let newDistance = distance + Map.find p map
                    (newDistance, p, d, c)
                let withDist = List.map mapper nextPoints
                let newDistances = List.fold (fun m (d, p, _, _) -> Map.add p d m) distances withDist
                // let nexts = List.fold (fun q p -> Set.add p q) (Set.remove elem queue) withDist
                let nexts = Set.union (Set.remove elem queue) (Set.ofList withDist)
                run nexts newDistances (Set.add current seen)
    run (Set.ofSeq [(0, (-1, 0), East, 3)]) (Map.ofList [(0, 0), 2]) Set.empty

let run lines =
    let grid = Grid2D.fromLines lines |> Grid2D.map (fun _ (v: char) -> int (v - '0'))
    let minDistance = traverse grid
    printfn "part 1: %d" minDistance
    ()

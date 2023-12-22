module Day17

open Utils
open FSharpx.Collections

let getNextPoints point dir count =
    let nextPossibility d = (Direction.moveTo point d, d, 2)
    let nextPoints = List.filter (fun d -> d <> dir && d <> (Direction.inv dir)) Direction.all |> List.map nextPossibility
    if count = 0 then nextPoints
    else (Direction.moveTo point dir, dir, count - 1) :: nextPoints


let outputDistances { map = map; height = height; width = width } distances =
    let find p =
        let filtered = Map.filter (fun (p', _, _) _ -> p = p') distances
        if Map.isEmpty filtered then None else (Some (Map.values filtered |> Seq.min))
    let mapper p _ = find p |> Option.map (fun v -> sprintf "%3d" v) |> Option.defaultValue "XX"
    let disMap = Map.map mapper map
    let disGrid = { map = disMap; height = height; width = width }
    printfn "%s" (Grid2D.toString "XX" disGrid)

let traverse ({ map = map; height = height; width = width } as grid) =
    let endPoint = (width - 1, height - 1)
    let rec run queue distances seen =
        if Set.isEmpty queue then failwith "could not find path"
        else
            let ((distance, (current, dir, count)) as elem) = Set.minElement queue
            if current = endPoint then
                distance
            else
                let nextSeen = Set.add (current, dir, count) seen
                let pred ((p, _, _) as key) = Map.containsKey p map && not (Set.contains key nextSeen)
                let nextPoints = getNextPoints current dir count |> List.filter pred
                let mapper ((p, _, _) as key) =
                    let currentDistance = Map.tryFind key distances |> Option.defaultValue 99999999
                    let newDistance = distance + Map.find p map
                    (min currentDistance newDistance, key)
                let withDist = List.map mapper nextPoints
                let newDistances = List.fold (fun m (d, key) -> Map.add key d m) distances withDist
                let nexts = Set.union (Set.remove elem queue) (Set.ofList withDist)
                run nexts newDistances nextSeen
    let result = run (Set.ofSeq [(0, ((-1, 0), East, 3))]) Map.empty Set.empty
    result - Map.find (0, 0) map

let run lines =
    let grid = Grid2D.fromLines lines |> Grid2D.map (fun _ (v: char) -> int (v - '0'))
    let minDistance = traverse grid
    printfn "part 1: %d" minDistance
    ()

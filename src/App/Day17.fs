module Day17

open Utils

let getNextPoints point dir count =
    let nextPossibility d = (Direction.moveTo point d, d, 1)
    let nextPoints = List.filter (fun d -> d <> dir && d <> (Direction.inv dir)) Direction.all |> List.map nextPossibility
    if count = 3 then nextPoints
    else (Direction.moveTo point dir, dir, count + 1) :: nextPoints

let getNextPointsLong point dir count =
    let nextPossibility c d = (Direction.moveTo point d, d, c)
    if count <= 3
        then [nextPossibility (count + 1) dir]
    else
        let nextPoints = List.filter (fun d -> d <> dir && d <> (Direction.inv dir)) Direction.all |> List.map (nextPossibility 1)
        if count = 10 then nextPoints
        else nextPossibility (count + 1) dir :: nextPoints

let traverse { map = map; height = height; width = width } getNexts =
    let endPoint = (width - 1L, height - 1L)
    let rec run queue distances seen =
        if Set.isEmpty queue then failwith "could not find path"
        else
            let ((distance, (current, dir, count)) as elem) = Set.minElement queue
            if current = endPoint then distance
            else
                let nextSeen = Set.add (current, dir, count) seen
                let pred ((p, _, _) as key) = Map.containsKey p map && not (Set.contains key nextSeen)
                let nextPoints = getNexts current dir count |> List.filter pred
                let mapper ((p, _, _) as key) =
                    let currentDistance = Map.tryFind key distances |> Option.defaultValue 99999999
                    let newDistance = distance + Map.find p map
                    (min currentDistance newDistance, key)
                let withDist = List.map mapper nextPoints
                let newDistances = List.fold (fun m (d, key) -> Map.add key d m) distances withDist
                let nexts = Set.union (Set.remove elem queue) (Set.ofList withDist)
                run nexts newDistances nextSeen
    let result = run (Set.ofSeq [(0, ((-1, 0), East, 0))]) Map.empty Set.empty
    result - Map.find (0L, 0L) map

let run lines =
    let grid = Grid2D.fromLines lines |> Grid2D.map (fun _ (v: char) -> int (v - '0'))
    printfn "part 1: %d" (traverse grid getNextPoints)
    printfn "part 2: %d" (traverse grid getNextPointsLong)

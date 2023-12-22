module Day21

open Utils

let parseLines lines =
    let grid = Grid2D.fromLines lines
    let start = Map.findKey (fun _ v -> v = 'S') grid.map
    ({ grid with map = Map.add start '.' grid.map }, start)

let getCandidates grid position =
    let neighbors = Grid2D.neighbors false position grid
    neighbors |> List.filter (snd >> (=) '.') |> List.map fst |> Set.ofList

let getPotentialPositions grid start times =
    let rec run possibilities n =
        if n = 0
            then Set.count possibilities
            else
                let nextPossibilities = Set.unionMany (Set.map (getCandidates grid) possibilities)
                run nextPossibilities (n - 1)
    run (Set.ofList [(start)]) times

let run lines =
    let (grid, start) = parseLines lines
    let result = getPotentialPositions grid start 64
    printfn "part 1: %d" result

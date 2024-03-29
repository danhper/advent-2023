module Day21

open Utils

let parseLines lines =
    let grid = Grid2D.fromLines lines
    let start = Map.findKey (fun _ v -> v = 'S') grid.map
    ({ grid with map = Map.add start '.' grid.map }, start)

let inline (%!) a b = (a % b + b) % b

let getCandidates grid position =
    let neighbors = Point2D.neighbors false position
    let t (x, y) = (x %! grid.width, y %! grid.height)
    neighbors |> List.filter (fun p -> Map.find (t p) grid.map = '.')

let getPotentialPositions grid start times =
    let rec run possibilities n results evens odds =
        if n = times + 1L
            then results
            else
                let folder s p = List.fold (flip Set.add) s (getCandidates grid p)
                let nextCandidates = Set.fold folder Set.empty possibilities
                let (nexts, count, evens, odds) =
                    if n % 2L = 0L then (Set.difference nextCandidates odds, Set.count evens, Set.union evens possibilities, odds)
                    else (Set.difference nextCandidates evens, Set.count odds, evens, Set.union odds possibilities)
                run nexts (n + 1L) (Map.add n (int64 (Set.count possibilities + count)) results) evens odds
    run (Set.ofList [(start)]) 0L Map.empty Set.empty Set.empty

let solve grid start steps =
    let n = steps / grid.width
    let s = steps % grid.width
    let (i0, i1, i2) = (s, s + grid.width, s + grid.width * 2L)
    let result = getPotentialPositions grid start i2
    let (a0, a1, a2) = (Map.find i0 result, Map.find i1 result, Map.find i2 result)
    let (b0, b1, b2) = (a0, a1 - a0, a2 - a1)
    b0 + b1 * n + (n * (n - 1L) / 2L) * (b2 - b1)

let run lines =
    let (grid, start) = parseLines lines
    let result = getPotentialPositions grid start 64
    printfn "part 1: %d" (Map.find 64L result)
    printfn "part 2: %d" (solve grid start 26501365)

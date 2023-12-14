module Day11

open Utils

let expandDimension multiplier dims =
    let folder (newDims, delta, previous) (i, elem) =
        let newDelta = delta + if elem = previous then 0UL else (elem - previous - 1UL) * (multiplier - 1UL)
        ((i, elem + newDelta) :: newDims, newDelta, elem)
    let (newDim, _, _) = List.fold folder ([], 0UL, 0UL) dims
    List.rev newDim

let expandSpace multiplier galaxies =
    let expand galaxies f =
        let indexed = List.sortBy snd (List.mapi (fun i v -> (i, f v)) galaxies)
        expandDimension multiplier indexed |> List.sortBy fst |> List.map snd
    let expandedX = expand galaxies fst
    let expandedY = expand galaxies snd
    List.zip expandedX expandedY

let getPairs elems =
    let rec computePairs = function
    | [] -> []
    | x :: xs -> List.map (fun v -> (x, v)) xs :: computePairs xs
    List.concat (computePairs elems)

let solve expandedGalaxies =
    let galaxyPairs = getPairs expandedGalaxies
    let distances = List.map (fun (a, b) -> Point2D.manhattanDistanceU64 a b) galaxyPairs
    List.sum distances

let run lines =
    let galaxyGrid = Grid2D.fromLines lines |> _.map |> Map.filter (fun _ v -> v = '#')
    let galaxies = galaxyGrid |> Map.keys |> List.ofSeq |> List.map (fun (x, y) -> (uint64 x, uint64 y))
    let expandedGalaxies = expandSpace 2UL galaxies
    let superExpandedGalaxies = expandSpace 1000000UL galaxies
    printfn "part 1: %d" (solve expandedGalaxies)
    printfn "part 2: %d" (solve superExpandedGalaxies)

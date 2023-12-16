module Day10

open Utils

type Pipe = Vertical | Horizontal | BendNE | BendNW | BendSE | BendSW | Empty
module Pipe =
    let all = [Vertical; Horizontal; BendNE; BendNW; BendSE; BendSW]

    let tryOfChar = function
        | '|' -> Some(Vertical)
        | '-' -> Some(Horizontal)
        | 'L' -> Some(BendNE)
        | 'J' -> Some(BendNW)
        | '7' -> Some(BendSW)
        | 'F' -> Some(BendSE)
        | '.' -> Some(Empty)
        | _   -> None

    let directionsOf = function
        | Vertical -> [North; South]
        | Horizontal -> [East; West]
        | BendNE -> [North; East]
        | BendNW -> [North; West]
        | BendSW -> [South; West]
        | BendSE -> [South; East]
        | Empty -> []

let canMove dir dest =
    List.contains (Direction.inv dir) (Pipe.directionsOf dest)

let getCandidates point grid =
    let pipe = Map.find point grid
    let nextCoords = List.map (fun dir -> (dir, Direction.moveTo point dir)) (Pipe.directionsOf pipe)
    let pred (dir, nextPoint) =
        let destPipe = Map.tryFind nextPoint grid
        Option.map (canMove dir) destPipe |> Option.defaultValue false
    List.filter pred nextCoords |> List.map snd |> Set.ofList

let withStartPipe start grid =
    let pred pipe =
        let potentialMap = Map.add start pipe grid
        Set.count (getCandidates start potentialMap) = 2
    let pipe = List.find pred Pipe.all
    Map.add start pipe grid

let transformGrid grid =
    let start = Map.findKey (fun _ v -> v = 'S') grid
    let transform (point, char) = Pipe.tryOfChar char |> Option.map (fun pipe -> (point, pipe))
    let finalGrid = grid |> Map.toList |> List.choose transform |> Map.ofList |> withStartPipe start
    (start, finalGrid)

let findLoop start grid =
    let rec go current points seen =
        if current = start && List.length points > 2 then Some(points)
        else
            let allCandidates = getCandidates current grid
            let candidates = Set.difference allCandidates seen |> Set.toList
            List.tryPick (fun c -> go c (c :: points) (Set.add c seen)) candidates
    let startNeighbor = getCandidates start grid |> Set.toList |> List.head
    go startNeighbor [startNeighbor] (Set.ofList [startNeighbor]) |> Option.get |> Set.ofList


let isInPolygon polygon grid (x, y) =
    let maxX = Set.map fst polygon |> Set.maxElement
    let xsToTest = Seq.map (fun x' -> (x', y)) { x..maxX } |> Set.ofSeq
    let xsIntersect = Set.intersect polygon xsToTest
    let folder (count, enteredBy) point =
        let pipe = Map.find point grid
        match pipe, enteredBy with
        | Vertical, _ -> count + 1, West
        | BendNE, _ -> count + 1, North
        | BendSE, _ -> count + 1, South
        | BendSW, North -> count, West
        | BendSW, _ -> count + 1, West
        | BendNW, South -> count, West
        | BendNW, _ -> count + 1, West
        | Horizontal, _ -> count, enteredBy
        | Empty, _ -> failwith "empty not allowed"
    let intersectCount = fst (Set.fold folder (0, West) xsIntersect)
    intersectCount % 2 = 1

let part2 grid loop =
    let allPoints = Map.keys grid |> Set.ofSeq
    let polygonWithoutTangeants = Set.filter (fun p -> Map.find p grid <> Horizontal) loop
    let pointsIn = Seq.filter (isInPolygon polygonWithoutTangeants grid) (Set.difference allPoints loop)
    printfn "part 2: %d" (Seq.length pointsIn)

let run lines =
    let { map = rawMap } = Grid2D.fromLines lines
    let (start, map) = transformGrid rawMap
    let loop = findLoop start map
    printfn "part 1: %d" (Set.count loop / 2)
    part2 map loop

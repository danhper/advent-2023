module Day18

open Utils
open FSharpx.Collections
open FParsec

// 0 means R, 1 means D, 2 means L, and 3 means U
let getDirection = function
    | 'R' | '0' -> East
    | 'D' | '1' -> South
    | 'L' | '2' -> West
    | 'U' | '3' -> North
    | c -> failwithf "unknown char %c" c

let parseColorInstr (color: string) =
    let value = color.Substring(0, 5)
    (getDirection color[5], System.Convert.ToInt32(value, 16))

let pdir = anyChar |>> getDirection
let pcolor = pstring "(#" >>. many1Satisfy isHex .>> pchar ')' |>> parseColorInstr
let pinput = pdir .>> spaces .>>. puint64 .>> spaces .>>. pcolor |>> fun ((d, n), c) -> ((d, int n), c)

type Map = {
    points: Set<int * int>;
    bounds: (int * int) * (int * int);
}

let display { points = points; bounds = ((minX, maxX), (minY, maxY)) } =
    let getValue p = if Set.contains p points then "#" else "."
    let produceLine y = { minX..maxX } |> Seq.map (fun x -> getValue (x, y)) |> String.concat ""
    { minY..maxY } |> Seq.map produceLine |> String.concat "\n"

let constructMap instructions =
    let folder (points, position) (dir, distance) =
        let nextPos = Direction.moveToWithDelta position dir distance
        let internalFolder pts d = Set.add (Direction.moveToWithDelta position dir d) pts
        (Seq.fold internalFolder points { 0..distance - 1 }, nextPos)
    let points = List.fold folder (Set.empty, (0, 0)) instructions |> fst
    let (xs, ys) = (Set.map fst points, Set.map snd points)
    let xBounds = (Set.minElement xs, Set.maxElement xs)
    let yBounds = (Set.minElement ys, Set.maxElement ys)
    { points = points; bounds = (xBounds, yBounds) }

let countPoints { points = points } start =
    let rec run toVisit seen =
        if Queue.isEmpty toVisit then Set.count seen
        else
            let (p, rest) = Queue.uncons toVisit
            let candidates = Point2D.neighbors false p
            let pred p = not (Set.contains p seen || Set.contains p points)
            let nextPoints = List.filter pred candidates
            let newQueue = List.fold (fun acc p -> Queue.conj p acc) rest nextPoints
            let newSeen = List.fold (fun acc p -> Set.add p acc) seen nextPoints
            run newQueue newSeen
    run (Queue.ofList [start]) (Set.singleton start) + Set.count points

let part1 parsed =
    let instrs = List.map fst parsed
    let map = constructMap instrs
    let start =
        let ((_, _), (minY, _)) = map.bounds
        let topPoints = Set.filter (snd >> (=) minY) map.points
        let minTopX = Set.map fst topPoints |> Set.minElement
        (minTopX + 1, minY + 1)
    printfn "part 1: %d" (countPoints map start)

let run lines =
    let parsed = List.map (Parsing.runf pinput) lines
    part1 parsed


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
    (getDirection color[5], System.Convert.ToInt64(value, 16))

let pdir = anyChar |>> getDirection
let pcolor = pstring "(#" >>. many1Satisfy isHex .>> pchar ')' |>> parseColorInstr
let pinput = pdir .>> spaces .>>. pint64 .>> spaces .>>. pcolor

let computePoints instructions =
    let folder (points, position) (dir, distance) =
        let nextPos = Direction.moveToWithDelta position dir distance
        (position :: points, nextPos)
    List.fold folder (List.empty, (0L, 0L)) instructions |> fst |> List.rev

let computePartialArea ((x1, y1) as p1) ((x2, y2) as p2) =
    (x1 * y2 - y1 * x2) + Point2D.manhattanDistance p1 p2

let computeArea points =
    let inPairs = List.zip points (List.tail points @ [List.head points])
    let result = List.map (uncurry computePartialArea) inPairs
    List.sum result / 2L + 1L

let solve f parsed =
    let instrs = List.map f parsed
    let points = computePoints instrs
    computeArea points

let run lines =
    let parsed = List.map (Parsing.runf pinput) lines
    printfn "part 1: %d" (solve fst parsed)
    printfn "part 2: %d" (solve snd parsed)


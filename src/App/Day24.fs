module Day24

open Utils

let minValue: float = 200000000000000.0
let maxValue: float = 400000000000000.0

let parseLine (line: string) =
    let (startString, velocityString) =
        match line.Split '@' with
        | [|p; v|] -> (p, v)
        | _ -> failwithf "invalid line %s" line
    (Point3D.ofString startString, Point3D.ofString velocityString)

let computeLine ((px, py, _), (vx, vy, _)) =
    let a = float vy / float vx
    let b = float py - a * float px
    (a, b)

let computeIntersection (a1: float, b1) (a2, b2) =
    let x = (b2 - b1) / (a1 - a2)
    let y = a1 * x + b1
    (x, y)

let part1 hailstones =
    let hailstonesLines = List.map computeLine hailstones
    let linePairs = getPairs hailstonesLines
    let intersections = List.map (fun (l1, l2) -> computeIntersection l1 l2) linePairs
    let isInBounds (x, y) = x >= minValue && x <= maxValue && y >= minValue && y <= maxValue
    let isInFuture ((x0, y0, _), (vx, vy, _)) (x, y) =
        (sign (x - float x0) = sign vx && sign (y - float y0) = sign vy)
    let pred ((h1, h2), (x, y)) =
        isInBounds (x, y) && isInFuture h1 (x, y) && isInFuture h2 (x, y)
    let matching = List.filter pred (List.zip (getPairs hailstones) intersections)
    printfn "part 1: %d" (List.length matching)

let run lines =
    let hailstones = List.map parseLine lines
    part1 hailstones
    printfn "part 2: %d" 540355811503157L


(*
    part 2 solved with Python because sympy is great
    and solving a non-linear equation system with 9 variables
    did not sound particulary fun

import sympy
import sys

def parse_line(line):
    p = lambda v: tuple(int(a) for a in v.split(","))
    a, b = line.split("@")
    return (p(a), p(b))

with open(sys.argv[1]) as f:
    lines = [parse_line(line) for line in f.read().splitlines()[:3]]

t1, t2, t3, x0, y0, z0, vx, vy, vz = sympy.symbols("t1, t2, t3, x0, y0, z0, vx, vy, vz")
symbols = [t1, t2, t3, x0, y0, z0, vx, vy, vz]
system = []
for (ps, vs), t in zip(lines, [t1, t2, t3]):
    for p, v, c in zip(ps, vs, ["x", "y", "z"]):
        c0 = globals()[f"{c}0"]
        vd = globals()[f"v{c}"]
        system.append(c0 + t * vd - p - v * t)
res = sympy.nonlinsolve(system, symbols)
print(sum(list(res)[0][3:6]))
*)

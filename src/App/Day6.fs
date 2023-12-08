module Day6

open FParsec

let parseInts = skipCharsTillString ":" true 100 >>. spaces >>. sepEndBy1 puint64 spaces

// if we write T as the total time, d as the distance, and Tp as the time the button is pressed
// we simnply need to solve the following inequation:
// -Tp^2 + T*Tp - d >= 0
// so we have Δ = T^2 - 4d
// and the bounds given by (-T ± √Δ)/-2
// we add/subtract a tiny amount to avoid cases where the bound is an integer, since we want strictly greater or lower bounds
let solveEquation distance time =
    let discriminant = time * time - 4UL * distance
    let rootDiscriminant = sqrt (float discriminant)
    let lowerIntersec = (float time - rootDiscriminant) / (float 2) + 0.0001
    let upperIntersec = (float time + rootDiscriminant) / (float 2) - 0.0001
    (uint64 (ceil lowerIntersec), uint64 (floor upperIntersec))

let countPossibilities (distance, time) =
    let (lower, upper) = solveEquation distance time
    upper - lower + 1UL

let combineNumbers numbers =
    List.map string numbers |> String.concat "" |> uint64

let part1 distances times =
    let data = List.zip distances times
    let possibilities = List.map countPossibilities data
    let result = List.reduce ((*)) possibilities
    printfn "part 1: %d" result

let part2 distances times =
    let distance = combineNumbers distances
    let time = combineNumbers times
    let result = countPossibilities (distance, time)
    printfn "part 1: %d" result

let run lines =
    let times = Utils.Parsing.runf parseInts (List.head lines)
    let distances = Utils.Parsing.runf parseInts lines[1]

    part1 distances times
    part2 distances times

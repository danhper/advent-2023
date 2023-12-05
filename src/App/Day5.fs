module Day5

open FParsec

type RangeMap = {
    destination: uint64;
    source: uint64;
    length: uint64;
}

let inRange v (s, e) = v >= s && v <= e

module RangeMap =
    let ofList list =
        match list with
        | [destination; source; length] -> { destination = destination; source = source; length = length }
        | _ -> failwithf "wrong range format"

    let private mapRange value length src dest =
        if inRange value (src, src + length - 1UL)
            then Some(value - src + dest)
            else None
    
    let map value r = mapRange value r.length r.source r.destination

    let invMap value r = mapRange value r.length r.destination r.source

type RangeMapping = {
    name: string;
    ranges: RangeMap list;
}

module RangeMapping =
    let map m value =
        List.tryFind Option.isSome (List.map (RangeMap.map value) m.ranges)
            |> Option.flatten
            |> Option.defaultValue value
    let invMap m value = 
        match List.choose (RangeMap.invMap value) m.ranges with
        | [] -> [value]
        | result -> result

let pseeds = pstring "seeds: " >>. sepBy puint64 (pstring " ") .>> newline .>> newline
let pmappingName = many1Chars (satisfy ((<>) ' ')) .>> pstring " map:" .>> newline
let prange = sepBy1 puint64 (pstring " ") |>> RangeMap.ofList
let pmapping = pmappingName .>>. sepEndBy1 prange newline |>> fun (name, ranges) -> { name = name; ranges = ranges }
let parser = pseeds .>>. sepBy1 pmapping newline

let getSeedLocation rangeMappings seed =
    let folder acc m = RangeMapping.map m acc
    List.fold folder seed rangeMappings

let getClosestSeed seeds rangeMappings =
    let seedLocations = List.map (getSeedLocation rangeMappings) seeds
    List.min seedLocations

let part1 seeds rangeMappings =
    let closestSeed = getClosestSeed seeds rangeMappings
    printfn "part1: %d" closestSeed

let getRange list =
    match list with
    | [a; b] -> (a, a + b - 1UL)
    | _ -> failwithf "invalid range"

let findSeeds location invRangeMappings =
    let folder acc m =
        List.collect (RangeMapping.invMap m) acc
    List.fold folder [location] invRangeMappings

let seedExists seedRanges seed = List.exists (inRange seed) seedRanges

let part2 seeds rangeMappings =
    let seedRanges = List.chunkBySize 2 seeds |> List.map getRange
    let invRangeMappings = List.rev rangeMappings
    let predicate location = List.exists (seedExists seedRanges) (findSeeds location invRangeMappings)
    let closestLocation = Seq.find predicate (Seq.initInfinite uint64)
    printfn "part 2: %d" closestLocation

let run lines =
    let input = String.concat "\n" lines
    let (seeds, rangeMappings) = Utils.Parsing.runf parser input
    part1 seeds rangeMappings
    part2 seeds rangeMappings

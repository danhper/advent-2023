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

    let mapRange value r =
        if inRange value (r.source, r.source + r.length - 1UL)
            then Some (value - r.source + r.destination, r.source + r.length - value)
            else None

type RangeMapping = {
    name: string;
    ranges: RangeMap list;
}

module RangeMapping =
    let mapRange m value =
        let range = List.tryFind Option.isSome (List.map (RangeMap.mapRange value) m.ranges) |> Option.flatten
        match range with
        | Some r -> r
        | None ->
            let rangesAbove = m.ranges |> List.filter (fun r -> r.source > value)
            if List.isEmpty rangesAbove then (value, pown 2UL 63)
            else 
                let closestRange = List.minBy (_.source) rangesAbove
                (value, closestRange.source - value)


let pseeds = pstring "seeds: " >>. sepBy puint64 (pstring " ") .>> newline .>> newline
let pmappingName = many1Chars (satisfy ((<>) ' ')) .>> pstring " map:" .>> newline
let prange = sepBy1 puint64 (pstring " ") |>> RangeMap.ofList
let pmapping = pmappingName .>>. sepEndBy1 prange newline |>> fun (name, ranges) -> { name = name; ranges = ranges }
let parser = pseeds .>>. sepBy1 pmapping newline

let getRange list =
    match list with
    | [a; b] -> (a, b)
    | _ -> failwithf "invalid range"

let partialMapSeedRange (seed, size) rangeMappings =
    let folder (value, rangeSize) rangeMapping =
        let (newValue, size) = RangeMapping.mapRange rangeMapping value
        (newValue, min rangeSize size)
    List.fold folder (seed, size) rangeMappings

let rec mapSeedRange (seed, size) rangeMappings =
    let (value, rangeSize) = partialMapSeedRange (seed, size) rangeMappings
    if rangeSize >= size then value
    else
        min value (mapSeedRange (seed + rangeSize, size - rangeSize) rangeMappings)

let part2 seeds rangeMappings =
    let seedRanges = List.chunkBySize 2 seeds |> List.map getRange
    let minimums = List.map (fun sr -> mapSeedRange sr rangeMappings) seedRanges
    printfn "part 2: %d" (List.min minimums)

let part1 seeds rangeMappings =
    let seedRanges = List.map (fun s -> (s, 1UL)) seeds
    let minimums = List.map (fun sr -> mapSeedRange sr rangeMappings) seedRanges
    printfn "part 1: %d" (List.min minimums)

let run lines =
    let input = String.concat "\n" lines
    let (seeds, rangeMappings) = Utils.Parsing.runf parser input
    part1 seeds rangeMappings
    part2 seeds rangeMappings

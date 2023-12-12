module Day12

type Measurement = Ok | NotOk | Unknown
module Measurement =
    let ofChar = function
        | '.' -> Ok
        | '#' -> NotOk
        | '?' -> Unknown
        | _ -> failwith "invalid char"

let parseLine fm fg (line: string) =
    match line.Split ' ' with
    | [|measurements; groups|] ->
        let parsedMeasurements = List.map Measurement.ofChar (List.ofSeq (fm measurements))
        let parsedGroups = fg (List.map uint64 (List.ofArray (groups.Split ',')))
        (parsedMeasurements, parsedGroups)
    | _ -> failwith "invalid line"

let computePossibilities measurements groups =
    let mutable cache = ref Map.empty

    let rec runWithCache measurements groups currentGroup currentLength =
        let key = (measurements, groups, currentGroup, currentLength)
        match Map.tryFind key cache.Value with
        | Some(v) -> v
        | None ->
            let result = run measurements groups currentGroup currentLength
            cache.Value <- Map.add key result cache.Value
            result
    and run measurements groups currentGroup currentLength =
        match measurements, groups, currentGroup with
        | [], [], None -> 1UL
        | NotOk :: _, [], None -> 0UL
        | [], [], Some(g) -> if g = currentLength then 1UL else 0UL
        | [], _, _ -> 0UL

        | Ok :: ms, groups, None -> runWithCache ms groups None 0UL
        | Ok :: ms, groups, Some g ->
            if currentLength = g
                then runWithCache ms groups None 0UL
                else 0UL

        | NotOk :: ms, g :: gs, None -> runWithCache ms gs (Some g) 1UL
        | NotOk :: ms, groups, Some g ->
            if currentLength + 1UL <= g
                then runWithCache ms groups (Some g) (currentLength + 1UL)
                else 0UL

        | Unknown :: ms, groups, current ->
            let withOk = runWithCache (Ok :: ms) groups current currentLength
            let withNotOk = runWithCache (NotOk :: ms) groups current currentLength
            withOk + withNotOk

    run measurements groups None 0UL

let solve fm fg lines =
    let parsedLines = List.map (parseLine fm fg) lines
    List.map (fun (m, g) -> computePossibilities m g) parsedLines

let unfoldMeasurements measurements =
    List.replicate 5 measurements |> String.concat "?"

let unfoldGroups groups =
    List.replicate 5 groups |> List.concat

let run lines =
    let possibilities = solve id id lines
    printfn "part 1: %d" (List.sum possibilities)
    let unfoldedPossibilities = solve unfoldMeasurements unfoldGroups lines
    printfn "part 2: %d" (List.sum unfoldedPossibilities )

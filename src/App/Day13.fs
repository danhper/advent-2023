module Day13

open Utils

type Location = Row | Column

let splitLines lines =
    let folder (current, all) line =
        if line = "" then ([], List.rev current :: all)
        else (line :: current, all)
    let (last, all) = List.fold folder ([], []) lines
    List.rev (List.rev last :: all)

let isReflection elements index1 index2 =
    let max = Array.length elements
    let rec check i1 i2 =
        i1 < 0 || i2 >= max || (elements[i1] = elements[i2] && check (i1 - 1) (i2 + 1))
    check index1 index2

let find1DReflection exclude elements =
    let length = Array.length elements
    let indices = Seq.zip { 0..length - 2 } { 1..length - 1 }
    let pred (i1, i2) = (i1, i2) <> exclude && isReflection elements i1 i2
    Seq.tryFind pred indices

let findReflection exclude grid =
    let allCols = Grid2D.allCols grid
    let (colsExclude, rowsExclude) =
        match exclude with
        | Some (Row, e) -> (-1, -1), e
        | Some (Column, e) -> e, (-1, -1)
        | None -> (-1, -1), (-1, -1)
    match find1DReflection colsExclude allCols with
    | Some(v) -> Some (Column, v)
    | None ->
        let allRows = Grid2D.allRows grid
        find1DReflection rowsExclude allRows
        |> Option.map (fun v -> (Row, v))

let computeScore = function
    | Some (Column, (_, v)) -> v
    | Some (Row, (_, v)) -> v * 100
    | None -> failwith "no reflection found"

let inverseValue = function
    | '.' -> '#'
    | '#' -> '.'
    | c -> failwithf "invalid value %c" c

let findSmudgeReflection grid =
    let initialReflection = (findReflection None grid).Value
    let modifyGrid k v = { grid with map = Map.add k (inverseValue v) grid.map }
    let pred k v = findReflection (Some initialReflection) (modifyGrid k v) |> Option.isSome
    let smudgeKey = Map.findKey pred grid.map
    findReflection (Some initialReflection) (modifyGrid smudgeKey (Map.find smudgeKey grid.map))

let part1 grids =
    let scores = List.map (findReflection None >> computeScore) grids
    printfn "part 1: %d" (List.sum scores)

let part2 grids =
    let scores = List.map (findSmudgeReflection >> computeScore) grids
    printfn "part 2: %d" (List.sum scores)

let run lines =
    let grids = List.map Grid2D.fromLines (splitLines lines)
    part1 grids
    part2 grids

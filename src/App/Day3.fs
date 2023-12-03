module Day3

open Utils

let groupNumbers grid =
    let folder (numbers, currentNumber) ((_, y) as point, value) =
        match currentNumber with
        | [] when isDigit value -> (numbers, (point, value) :: currentNumber)
        | ((_, y'), _) :: _ when isDigit value && y = y' -> (numbers, (point, value) :: currentNumber)
        | [] -> (numbers, List.empty)
        | number -> (List.rev number :: numbers, List.empty)
    let orderedCells = Map.toList grid |> List.sortBy (fun ((x, y), _) -> (y, x))
    let (numbers, lastNumber) = List.fold folder (List.empty, List.empty) orderedCells
    if List.isEmpty lastNumber then numbers
    else List.rev lastNumber :: numbers

let hasAdjacentSymbol grid  point =
    let neighbors = Grid2D.neighbors point grid
    List.exists (fun (_, v) -> v <> '.' && not (isDigit v)) neighbors

let isGroupNumberRelevant grid groupNumber =
    List.exists (fst >> hasAdjacentSymbol grid) groupNumber

let numberToInt number =
    String.concat "" (List.map (snd >> string) number) |> int

let adjacentNumbers numbers point =
    let pred digits = List.exists (fun (p, _) -> List.contains point (Point2D.neighbors p)) digits
    List.filter pred numbers

let findGears grid =
    Map.filter (fun _ v -> v = '*') grid |> Map.keys |> Seq.toList

let part1 numbers =
    let ints = List.map numberToInt numbers
    printfn "part 1: %d" (List.sum ints)

let part2 grid numbers =
    let gears = findGears grid
    let validGears = List.filter (fun gear -> List.length (adjacentNumbers numbers gear) = 2) gears
    let processGear = adjacentNumbers numbers >> List.map numberToInt >> List.reduce (*)
    let result = List.sum (List.map processGear validGears)
    printfn "part 2: %d" result

let run lines =
    let grid = Grid2D.fromLines lines
    let numbers = List.filter (isGroupNumberRelevant grid) (groupNumbers grid)
    part1 numbers
    part2 grid numbers

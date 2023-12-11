module Day9

open Utils

let parseLine (line: string) = List.map int64 (List.ofArray (line.Split ' '))

let computeNextLine numbers =
    let folder (prev, newList) elem = (elem, (elem - prev) :: newList)
    let (_, result) = List.fold folder (0L, []) numbers
    List.rev result |> List.tail

let computeHeadsAndTails numbers =
    let rec run lines current =
        if List.forall ((=) 0L) current then lines
        else
            let next = computeNextLine current
            run ((List.head current, List.last current) :: lines) next
    run [] numbers |> List.unzip

let solve headsAndTails f =
    List.map f headsAndTails |> List.sum

let run lines =
    let headsAndTails = List.map (parseLine >> computeHeadsAndTails) lines
    let part1Result = solve headsAndTails (snd >> List.sum)
    let part2Result = solve headsAndTails (fst >> List.reduce (flip (-)))
    printfn "part 1: %d" part1Result
    printfn "part 2: %d" part2Result

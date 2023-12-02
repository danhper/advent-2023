module Day1

let digits =
    [ "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine" ]


let nextIndices current first last =
    match current, first with
    | (None, _) -> (first, last)
    | (Some _) as c, ((Some _) as f) -> (f, c)
    | (Some _) as c, None -> (c, c)

let startsWithNumber parse (line: string) number =
    line.StartsWith(string number) || (parse && line.StartsWith(digits[number - 1]))

let computeNext parse line (first, last) =
    let current = Seq.tryFind (startsWithNumber parse line) { 1..9 }
    nextIndices current first last

let getNumber parse (line: string) =
    let folder acc i = computeNext parse line[i..] acc
    let (first, last) = Seq.fold folder (None, None) { 0 .. line.Length - 1 }
    Option.get first * 10 + Option.get last

let solve lines parse =
    lines |> List.map (getNumber parse) |> List.sum

let run lines =
    printfn "part 1: %d" (solve lines false)
    printfn "part 2: %d" (solve lines true)

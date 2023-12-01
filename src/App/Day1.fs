module Day1

let digits =
    [ "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine" ]


let next_indices current first last =
    match current, first with
    | (None, _) -> (first, last)
    | (Some _) as c, ((Some _) as f) -> (f, c)
    | (Some _) as c, None -> (c, c)

let starts_with_number parse (line: string) number =
    line.StartsWith(string number) || (parse && line.StartsWith(digits[number - 1]))

let compute_next parse line (first, last) =
    let current = Seq.tryFind (starts_with_number parse line) { 1..9 }
    next_indices current first last

let get_number parse (line: string) =
    let folder acc i = compute_next parse line[i..] acc
    let (first, last) = Seq.fold folder (None, None) { 0 .. line.Length - 1 }
    Option.get first * 10 + Option.get last

let solve lines parse =
    lines |> List.map (get_number parse) |> List.sum

let run suffix =
    let lines = Utils.get_file_name 1 suffix |> Utils.read_lines
    printfn "part 1: %d" (solve lines false)
    printfn "part 2: %d" (solve lines true)

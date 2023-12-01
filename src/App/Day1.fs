module Day1

let digits =
    [ "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine" ]

let mapped_digits = List.mapi (fun i v -> ((string (i + 1)), v)) digits

let get_digit index_of extract_by line parse =
    let get_value v c =
        (v, index_of line (string v))
        :: if parse then [ (v, index_of line (string c)) ] else []

    let values = List.collect (fun (v, c) -> get_value v c) mapped_digits
    List.filter (fun (_, i) -> i <> -1) values |> extract_by snd |> fst

let extract_number parse (line: string) =
    let first_digit =
        get_digit (fun (s1: string) s2 -> s1.IndexOf(s2)) List.minBy line parse

    let last_digit =
        get_digit (fun (s1: string) s2 -> s1.LastIndexOf(s2)) List.maxBy line parse

    int (first_digit + last_digit)


let solve lines parse =
    lines |> List.map (extract_number parse) |> List.sum

let run suffix =
    let lines = Utils.get_file_name 1 suffix |> Utils.read_lines
    printfn "part 1: %d" (solve lines false)
    printfn "part 2: %d" (solve lines true)

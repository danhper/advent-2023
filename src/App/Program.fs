let run day suffix =
    let lines = Utils.IO.getFileName day suffix |> Utils.IO.readLines
    match day with
    | "1" -> Day1.run lines
    | "2" -> Day2.run lines
    | "3" -> Day3.run lines
    | "4" -> Day4.run lines
    | "5" -> Day5.run lines
    | n -> failwithf "error: unknown day %s" n

let get_args args =
    match args with
    | [| day |] -> (day, "")
    | [| day; suffix |] -> (day, suffix)
    | _ -> failwith "usage: dotnet run <day> [--debug]"

[<EntryPoint>]
let main args =
    try
        let (day, suffix) = get_args args
        run day suffix
        0
    with msg ->
        printfn "%s" msg.Message
        1

﻿let run day =
    match day with
    | "1" -> Day1.run
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

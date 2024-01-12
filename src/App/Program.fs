let run day suffix =
    let lines = Utils.IO.getFileName day suffix |> Utils.IO.readLines

    match day with
    | "1" -> Day1.run lines
    | "2" -> Day2.run lines
    | "3" -> Day3.run lines
    | "4" -> Day4.run lines
    | "5" -> Day5.run lines
    | "6" -> Day6.run lines
    | "7" -> Day7.run lines
    | "8" -> Day8.run lines
    | "9" -> Day9.run lines
    | "10" -> Day10.run lines
    | "11" -> Day11.run lines
    | "12" -> Day12.run lines
    | "13" -> Day13.run lines
    | "14" -> Day14.run lines
    | "15" -> Day15.run lines
    | "16" -> Day16.run lines
    | "17" -> Day17.run lines
    | "18" -> Day18.run lines
    | "19" -> Day19.run lines
    | "20" -> Day20.run lines
    | "21" -> Day21.run lines
    | "22" -> Day22.run lines
    | "23" -> Day23.run lines
    | "24" -> Day24.run lines
    | "25" -> Day25.run lines
    | n -> failwithf "error: unknown day %s" n

let getArgs args =
    match args with
    | [| day |] -> (day, "")
    | [| day; suffix |] -> (day, suffix)
    | _ -> failwith "usage: dotnet run <day> [--debug]"

let runDay day suffix =
    let stopwatch = System.Diagnostics.Stopwatch()
    stopwatch.Start()
    run day suffix
    printfn "Day %s executed in %dms" day stopwatch.ElapsedMilliseconds

[<EntryPoint>]
let main args =
    try
        let (day, suffix) = getArgs args
        if day = "--all" then
            Seq.iter (fun d -> runDay d suffix) (Seq.map string { 1..25 })
            else runDay day suffix
        0
    with msg ->
        printfn "%s" msg.Message
        1

namespace Utils

    [<AutoOpen>]
    module Core =
        let log a =
            printfn "%A" a
            a
        
        let isDigit = System.Char.IsDigit

    module IO = 
        open System.IO
        let dataDir =
            Path.Combine(Directory.GetParent(__SOURCE_DIRECTORY__).Parent.ToString(), "data")

        let readLines file_name =
            Path.Combine(dataDir, file_name) |> File.ReadAllLines |> Array.toList

        let getFileName day (suffix: string) =
            if suffix.Length = 0 then
                sprintf "day%s.txt" day
            else
                sprintf "day%s-%s.txt" day suffix


    module Parsing =
        open FParsec
        let runf p str =
            match run p str with
            | Success(result, _, _)   -> result
            | Failure(errorMsg, _, _) -> failwithf "Parse failed: %s" errorMsg

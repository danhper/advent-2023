module Utils

open System.IO

let data_dir =
    Path.Combine(Directory.GetParent(__SOURCE_DIRECTORY__).Parent.ToString(), "data")


let read_lines file_name =
    Path.Combine(data_dir, file_name) |> File.ReadAllLines |> Array.toList


let get_file_name day (suffix: string) =
    if suffix.Length = 0 then
        sprintf "day%d.txt" day
    else
        sprintf "day%d-%s.txt" day suffix

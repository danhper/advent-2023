module Day15

type Operation =
    | Add of string * int
    | Remove of string

let computeHash value =
    let folder acc char = (acc + (uint64 char)) * 17UL
    let total = List.fold folder 0UL (List.ofSeq value)
    int (total % 256UL)

type ('a, 'b) IndexedList = ('a * 'b) list
module IndexedList =
    let empty  = []

    let add key value list =
        let rec run = function
            | [] -> [(key, value)]
            | (k, _) as x :: xs ->
                if k = key then (k, value) :: xs
                else x :: (run xs)
        run list

    let remove key list =
        match List.tryFindIndex (fst >> ((=) key)) list with
        | None -> list
        | Some i -> List.removeAt i list

type HashMap = (string * int) list array
module HashMap =
    let create size: HashMap = Array.create size IndexedList.empty

    let add key value (map: HashMap) =
        let slot = computeHash key
        let slotContent = map[slot]
        map[slot] <- IndexedList.add key value slotContent
        map
    
    let remove key (map: HashMap) =
        let slot = computeHash key
        let slotContent = map[slot]
        map[slot] <- IndexedList.remove key slotContent
        map
    
    let computeScore (map: HashMap) =
        let folder acc (i, slotContent) =
            let indexed = List.indexed slotContent
            let inFolder s (j, (_, v)) = s + (i + 1) * (j + 1) * v
            acc + List.fold inFolder 0 indexed
        Array.fold folder 0 (Array.indexed map)

let parseInput (input: string) =
    match input.Split '=' with
    | [|k; v|] -> Add (k, int v)
    | _ -> match input.Split '-' with
           | [|k; ""|] -> Remove k
           | _ -> failwithf "failed to parse input %s" input

let buildHashMap inputs =
    let folder acc input =
        match input with
        | Add (k, v) -> HashMap.add k v acc
        | Remove k -> HashMap.remove k acc
    List.fold folder (HashMap.create 256) inputs

let part2 inputs =
    let parsedInputs = List.map parseInput inputs
    let map = buildHashMap parsedInputs
    printfn "part 2: %d" (HashMap.computeScore map)

let part1 inputs =
    let hashes = List.map computeHash inputs
    printfn "part 1: %d" (List.sum hashes)

let run lines =
    let inputs = List.head lines |> (fun (v: string) -> v.Split ",") |> List.ofArray
    part1 inputs
    part2 inputs

module Day2

open FParsec

type Game = { id: int; rounds: list<Map<string, int>>; }

let pcolor = many1Satisfy System.Char.IsLetter
let pcube = (pint64 .>> pchar ' ' .>>. pcolor) |>> fun (a, b) -> (b, int a)
let pround = sepBy1 pcube (pstring ", ") |>> Map.ofList
let prounds = sepBy1 pround (pstring "; ")
let pgame = pstring "Game " >>. pint64 .>> pstring ": " .>>. prounds
           |>> fun (i, r) -> { id = int i; rounds = r }

let isGamePossible available game =
    List.forall (Map.forall (fun k v -> Map.find k available >= v)) game.rounds

let minimumColor =
    let optionMax v ov = max v (Option.defaultValue v ov)
    let mergeKey acc k v = Map.add k (optionMax v (Map.tryFind k acc)) acc
    List.reduce (Map.fold mergeKey)

let power = Map.values >> Seq.reduce ((*))

let part1 games =
    let available = Map.ofList [("red", 12); ("green", 13); ("blue", 14)]
    let possible = List.filter (isGamePossible available) games
    printfn "part 1: %d" (List.sumBy (_.id) possible)

let part2 games =
    let powers = List.map (_.rounds >> minimumColor >> power) games
    printfn "part 2: %d" (List.sum powers)

let run lines =
    let games = List.map (Utils.Parsing.runf pgame) lines
    part1 games
    part2 games

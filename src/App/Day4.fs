module Day4

open Utils
open FParsec

type Card = {
    winning: int Set;
    numbers: int Set;
}

let pnumbers = sepEndBy pint64 spaces |>> List.map int
let pcardId = pstring "Card" >>. spaces >>. pint64 .>> pchar ':' .>> spaces |>> int
let phand = pnumbers .>> pchar '|' .>> spaces .>>. pnumbers
let pcard = pcardId .>>. phand

let parseLine line =
    let (cardId, (winning, numbers)) = Parsing.runf pcard  line
    (cardId, { winning = Set.ofList winning; numbers = Set.ofList numbers })

let matchingCount { winning = winning; numbers = numbers } =
    Set.intersect winning numbers |> Set.count

let computeScore card =
    let winnersCount = matchingCount card
    if winnersCount = 0 then 0
    else pown 2 (winnersCount - 1)

let computeScratches cards =
    let updateHand m id count score =
        let addCount = Option.map ((+) count)
        Seq.fold (fun m n -> Map.change (id + n) addCount m) m { 1..score }
    let folder m id card =
        let score = matchingCount card
        let count = Map.find id m
        updateHand m id count score
    let initialHand = Map.map (fun _ _ -> 1) cards
    Map.fold folder initialHand cards


let run lines =
    let cards = List.map parseLine lines |> Map.ofList

    let scores = Seq.map computeScore (Map.values cards)
    printfn "part 1: %d" (Seq.sum scores)

    let scratches = computeScratches cards
    printfn "part 2: %d" (Seq.sum (Map.values scratches))


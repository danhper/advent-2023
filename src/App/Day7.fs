module Day7

let cardStrength hasJoker c =
    match c with
    | 'A' -> 14
    | 'K' -> 13
    | 'Q' -> 12
    | 'J' when hasJoker -> 0
    | 'J' when not hasJoker -> 11
    | 'T' -> 10
    | _ -> int (c - '0')

let groupHand hand =
    let addOne = Option.map ((+) 1) >> Option.orElse (Some 1)
    let folder acc c = Map.change c addOne acc
    List.fold folder Map.empty hand

let handStrength hasJoker hand =
    let grouped = groupHand hand
    let groupsCount = Map.count grouped
    let largestGroup = Seq.max (Map.values grouped)
    let jokerCount = if hasJoker then Map.tryFind 'J' grouped |> Option.defaultValue 0 else 0
    match (largestGroup, groupsCount, jokerCount) with
    | 5, _, _ -> 7 // five of a kind
    | 4, _, 1 -> 7 // five of a kind
    | 3, _, 2 -> 7 // five of a kind
    | _, 2, 3 -> 7 // five of a kind
    | _, _, 4 -> 7 // five of a kind

    | 4, _, _ -> 6 // four of a kind
    | 3, _, 1 -> 6 // four of a kind
    | 2, 3, 2 -> 6 // four of a kind
    | _, _, 3 -> 6 // four of a kind

    | 3, 2, _ -> 5 // full house
    | 2, 3, 1 -> 5 // full house

    | 3, _, _ -> 4 // three of a kind
    | 2, _, 1 -> 4 // three of a kind
    | _, _, 2 -> 4 // three of a kind

    | 2, 3, _ -> 3 // two pairs
    | _, 4, 1 -> 3 // two pairs

    | 2, _, _ -> 2 // one pair
    | _, _, 1 -> 2 // one pair

    | _, _, _ -> 1 // high card

let compareCards hasJoker hand1 hand2 =
    let computeStrength = cardStrength hasJoker
    let diff = List.skipWhile (fun (x, y) -> computeStrength x = computeStrength y) (List.zip hand1 hand2)
    match diff with
    | [] -> 0
    | (x, y):: _ -> if computeStrength x < computeStrength y then -1 else 1

let compareHands hasJoker hand1 hand2 =
    match handStrength hasJoker hand1, handStrength hasJoker hand2 with
    | s1, s2 when s1 = s2 -> compareCards hasJoker hand1 hand2
    | s1, s2 when s1 < s2 -> -1
    | _ (* s1 > s2 *) -> 1
let compareLines hasJoker (hand1, _) (hand2, _) = compareHands hasJoker hand1 hand2

let parseLine (line: string) =
    match line.Split(" ") with
    | [|hand; bid|] -> (List.ofSeq hand, int bid)
    | _ -> failwithf "invalid line"
 

let computeWinnings hasJoker hands =
    let sortedHands = List.sortWith (compareLines hasJoker) hands
    List.mapi (fun i (_, bid) -> (i + 1) * bid) sortedHands

let run lines =
    let hands = List.map parseLine lines
    printfn "part 1: %d" (List.sum (computeWinnings false hands))
    printfn "part 2: %d" (List.sum (computeWinnings true hands))

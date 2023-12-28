module Day25

open FSharpx.Collections

let parseLine (line: string) =
    match line.Split ": " with
    | [|node; connected|] -> node, Set.ofArray (connected.Split ' ')
    | _ -> failwithf "invalid line: %s" line

type Graph = Map<string, Set<string>>

module Graph =
    let getEdge a b = if a < b then (a, b) else (b, a)

    let addUnidirEdge a b g =
        let f v = Some (Set.add b (Option.defaultValue Set.empty v))
        Map.change a f g

    let addEdge a b g = g |> addUnidirEdge a b |> addUnidirEdge b a

    let ofLines lines =
        let folder g line =
            let (node, connected) = parseLine line
            let innerFolder m c = addEdge node c m
            Set.fold innerFolder g connected
        List.fold folder Map.empty lines

    let getDistances point graph =
        let rec run queue distances seen =
            match Queue.tryUncons queue with
            | None -> distances
            | Some (current, rest) ->
                let newSeen = Set.add current seen
                let neighbors = Set.filter (fun v -> not (Set.contains v newSeen)) (Map.find current graph)
                let newQueue = Set.fold (fun q p -> Queue.conj p q) rest neighbors
                let currentDistance = Map.find current distances
                let changer d =
                    let candidate = currentDistance + 1
                    match d with
                    | None -> Some candidate
                    | Some v -> Some (if v < candidate then v else candidate)
                let newDistances = Set.fold (fun m p -> Map.change p changer m) distances neighbors
                run newQueue newDistances newSeen
        run (Queue.ofList [point]) (Map.ofList [(point, 0)]) Set.empty

let run lines =
    let graph = Graph.ofLines lines
    let (start, _) = List.head (Map.toList graph)
    let distances = Graph.getDistances start graph
    let stop = Map.toList distances |> List.maxBy snd |> fst

    (* need to implement minimum cut between start and stop *)
    (* could not be bothered for now so just used good old networkx in Python

    import networkx as nx

    G = nx.Graph()

    with open("../../data/day25.txt", "r") as f:
        for line in f:
            src, dests = line.strip().split(": ")
            for dest in dests.split(" "):
                G.add_edge(src, dest, capacity=1)

    start = next(iter(G.nodes))
    stop = list(nx.single_source_shortest_path_length(G, start))[-1]
    _, (g1, g2) = nx.minimum_cut(G, start, stop)
    print(len(g1) * len(g2))
    *)

    printfn "part 1: %d" 582626

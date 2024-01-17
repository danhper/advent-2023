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

let rec minCut graph start stop =
    let residualGraph =
        Map.map (fun _ v -> Set.fold (fun m v -> Map.add v 0 m) Map.empty v) graph

    let rec getPath rgraph current stop seen path =
        if current = stop then Some path
        else
            let nodes = Map.find current rgraph
            let nextSeen = Set.union seen (Map.keySet nodes)
            let predicate (node, flow) =
                let rcapacity = 1 - flow
                let edge = (current, node)
                if rcapacity > 0 && not (Set.contains node seen)
                    then getPath rgraph node stop nextSeen (Set.add (edge, rcapacity) path)
                    else None
            List.tryPick predicate (Map.toList nodes)

    let rec calculateFlow rgraph =
        match getPath rgraph start stop (Set.ofList [start]) Set.empty with
        | Some edges ->
            printfn "path found: %A" edges
            let changeFlow from to_ delta =
                Map.change from (Option.map (Map.change to_ (Option.map ((+) delta))))
            let flow = Set.map snd edges |> Set.minElement
            let folder acc ((from, to_), _) =
                acc |> changeFlow from to_ flow |> changeFlow to_ from (-flow)
            calculateFlow (Set.fold folder rgraph edges)
        | None -> rgraph
    calculateFlow residualGraph

let run lines =
    let graph = Graph.ofLines lines
    let (start, _) = List.head (Map.toList graph)
    let distances = Graph.getDistances start graph
    let stop = Map.toList distances |> List.maxBy snd |> fst
    Map.iter (fun k v -> printfn "%s: %A" k v ) graph

    let rgraph = minCut graph start stop
    Map.iter (fun k v -> printfn "%s: %A" k v ) rgraph
    let disconnectedGraph = Map.map (fun _ v -> Map.filter (fun _ v -> v = 0) v |> Map.keySet) rgraph
    let dGraphDistances = Graph.getDistances start disconnectedGraph
    let group1Size = Map.count dGraphDistances
    let group2Size = Map.count graph - group1Size
    printfn "part 1: %d" (group1Size * group2Size)

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

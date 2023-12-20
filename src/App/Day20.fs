module Day20

open FParsec
open Utils

type Signal = Low | High

type CircuitModule =
    | Broadcaster
    | FlipFlop of Signal * bool
    | Conjuction of Map<string, Signal>

type NamedModule = string * CircuitModule
type Pulse = Signal * string * string
type Circuit = {
    modules: Map<string, CircuitModule>;
    edges: Map<string, string list>;
}

let pbroadcaster = pstring "broadcaster" |>> fun _ -> ("broadcaster", Broadcaster)
let pflipFlop = pchar '%' >>. many1Satisfy isAsciiLetter |>> fun s -> (s, FlipFlop (High, false))
let pconjuction = pchar '&' >>. many1Satisfy isAsciiLetter |>> fun s -> (s, Conjuction Map.empty)
let pmoduleType = pbroadcaster <|> pconjuction <|> pflipFlop
let pmodule = pmoduleType .>> pstring " -> " .>>. sepBy1 (many1Satisfy isAsciiLetter) (pstring ", ")

let processPulse { modules = modules } (signal, srcName, destName) =
    let destModule = Map.find destName modules
    match destModule, signal with
    | Broadcaster, _ -> Broadcaster
    | FlipFlop (_, o), High -> FlipFlop (High, o)
    | FlipFlop (_, o), Low -> FlipFlop (Low, not o)
    | Conjuction c, _ -> Conjuction (Map.add srcName signal c)

let processPulses pulses = List.map processPulse pulses

let getNextPulses { modules = modules; edges = edges } cname =
    let makePulses signalType = List.map (fun m -> (signalType, cname, m)) (Map.find cname edges)
    match Map.tryFind cname modules with
    | None
    | Some (FlipFlop (High, _)) ->  []
    | Some (FlipFlop (Low, o)) ->
        let signalType = if o then High else Low
        makePulses signalType
    | Some (Conjuction inputs) ->
        let signalType = if Map.forall (fun _ v -> v = High) inputs then Low else High
        makePulses signalType
    | Some Broadcaster -> makePulses Low

let parseCircuit lines =
    let folder { modules = modules; edges = edges } line =
        let ((name, m), es) = Utils.Parsing.runf pmodule line
        { modules = Map.add name m modules; edges = Map.add name es edges }
    let circuit = List.fold folder { modules = Map.empty; edges = Map.empty } lines

    let updateModule ms inputName outputName =
        match Map.find outputName ms with
        | Conjuction m -> Conjuction (Map.add inputName Low m)
        | m -> m
    let edgeFolder acc srcName dstNames =
        let relevantDsts = List.filter (fun d -> Map.containsKey d acc) dstNames
        List.fold (fun a dst -> Map.add dst (updateModule a srcName dst) a) acc relevantDsts

    { circuit with modules = Map.fold edgeFolder circuit.modules circuit.edges }


let processRound circuit f =
    let rec run circ pulses =
        match pulses with
        | [] -> circ
        | pulses :: rest ->
            f pulses
            let folder mods ((_, _, name) as pulse) =
                Map.change name (Option.map (fun _ -> processPulse circ pulse)) mods
            let newModules = List.fold folder circ.modules pulses
            let newCircuit = { circ with modules = newModules }
            let newPulses = List.map (fun (_, _, nm) -> getNextPulses newCircuit nm) pulses
            run newCircuit (rest @ newPulses)
    let initialPulses = [[(Low, "",  "broadcaster")]]
    run circuit initialPulses

let part1 circuit =
    let (low, high) = (ref 0UL, ref 0UL)
    let handlePulses pulses =
        let (lowPulses, highPulses) = List.partition (fun (p, _, _) -> p = Low) pulses
        low.Value <- low.Value + uint64 (List.length lowPulses)
        high.Value <- high.Value + uint64 (List.length highPulses)
    let folder circ _ = processRound circ handlePulses
    let _ = Seq.fold folder circuit {0..999}
    printfn "part 1: %d" (low.Value * high.Value)

let rec findClosestConjunction circuit key =
    let input = Map.findKey (fun _ edges -> List.exists ((=) key) edges) circuit.edges
    match Map.find input circuit.modules with
    | Conjuction _ -> input
    | _ -> findClosestConjunction circuit input

let part2 circuit =
    let rxInput = findClosestConjunction circuit "rx"
    let length = match Map.find rxInput circuit.modules with
                 | Conjuction m -> Map.count m
                 | _ -> failwith "expected conjuction"

    let conjuctionFound = ref Map.empty
    let handlePulses n pulses =
        let f (v, s, d) =
            if v = High && d = rxInput && not (Map.containsKey s conjuctionFound.Value)
                then conjuctionFound.Value <- Map.add s n conjuctionFound.Value
        List.iter f pulses

    let rec run circ n =
        let newCirc = processRound circ (handlePulses n)
        if Map.count conjuctionFound.Value < length then run newCirc (n + 1UL)
    run circuit 1UL

    let result = Seq.reduce lcm (Map.values conjuctionFound.Value)
    printfn "part 2: %d" result

let run lines =
    let circuit = parseCircuit lines
    part1 circuit
    part2 circuit

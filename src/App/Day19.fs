module Day19

open FParsec

type CompOperator = Lt | Gt

type Expression = Variable of string | Constant of uint64
and Statement = Accept | Reject | Jump of string | Condition of Condition
and Predicate = CompOperator * Expression * Expression
and Condition = {
    predicate: Predicate;
    ifTrue: Statement;
    ifFalse: Statement;
}
type Constraint = Predicate * bool

type Env = {
    variables: Map<string, uint64>;
    statements: Map<string, Statement>;
}

let pvar = many1Satisfy isAsciiLower |>> Variable
let pop = pchar '>' <|> pchar '<' |>> fun c -> if c = '<' then Lt else Gt
let pexpr = pvar <|> (puint64 |>> Constant)
let ppred = pexpr .>>. pop .>>. pexpr |>> fun ((left, op), right) -> (op, left, right)
let paccept = pchar 'A' |>> fun _ -> Accept
let preject = pchar 'R' |>> fun _ -> Reject
let pjump = many1Satisfy isAsciiLower |>> Jump
let pcond, pcondRef = createParserForwardedToRef<Statement, unit>()
let pstmt = paccept <|> preject <|> (attempt pcond) <|> pjump
pcondRef.Value <- ppred .>> pchar ':' .>>. pstmt .>> pchar ',' .>>. pstmt
    |>> fun ((pred, ifTrue), ifFalse) -> Condition { predicate = pred; ifTrue = ifTrue; ifFalse = ifFalse }
let prule = many1Satisfy isAsciiLower .>> pchar '{' .>>. pstmt .>> pchar '}'
let prules = sepEndBy1 prule (pchar '\n') |>> Map.ofList

let passign = many1Satisfy isAsciiLower .>> pchar '=' .>>. puint64
let passignSet = pchar '{' >>. sepBy1 passign (pchar ',') .>> pchar '}' |>> Map.ofList
let passigns = sepEndBy1 passignSet (pchar '\n')
let pinput = prules .>> pchar '\n' .>>. passigns


let evalExpr env = function
    | Variable s -> Map.find s env.variables
    | Constant v -> v

let evalPredicate op left right =
    match op with
    | Lt when left < right -> true
    | Gt when left > right -> true
    | _ -> false

let rec eval env = function
    | Accept -> true
    | Reject -> false
    | Condition { predicate = (op, left, right); ifTrue = ifTrue; ifFalse = ifFalse } ->
        if evalPredicate op (evalExpr env left) (evalExpr env right)
            then eval env ifTrue
            else eval env ifFalse
    | Jump s -> eval env (Map.find s env.statements)

let rec symbolicEval statements stmt =
    let rec run constraints = function
        | Accept -> [constraints]
        | Reject -> []
        | Jump s -> run constraints (Map.find s statements)
        | Condition { predicate = predicate; ifTrue = ifTrue; ifFalse = ifFalse } ->
            run ((predicate, true) :: constraints) ifTrue @ run ((predicate, false) :: constraints) ifFalse
    run [] stmt

let solveConstraints constraints =
    let initState = List.fold (fun m e -> Map.add e (1UL, 4000UL) m) Map.empty ["x"; "m"; "a"; "s"]
    let decreaseMax newMax (currentMin, currentMax) = (currentMin, min currentMax newMax)
    let increaseMin newMin (currentMin, currentMax) = (max currentMin newMin, currentMax)
    let folder m (pred, b) =
        match pred, b with
        | (Lt, Variable v, Constant c), true -> Map.change v (Option.map (decreaseMax (c - 1UL))) m
        | (Lt, Variable v, Constant c), false -> Map.change v (Option.map (increaseMin c)) m
        | (Gt, Variable v, Constant c), true -> Map.change v (Option.map (increaseMin (c + 1UL))) m
        | (Gt, Variable v, Constant c), false -> Map.change v (Option.map (decreaseMax c)) m
        | _ -> failwithf "unexpected predicate %A" pred
    List.fold folder initState constraints


let rangeSize (s, e) = e - s + 1UL

let constraintPossibilitiesCount c = Map.fold (fun acc _ r -> acc * rangeSize r) 1UL c

(* NOTE: this does not account for overlapping regions
    It seems to work for this problem but if we need to account for these, we could fairly easily
    fix this using inclusion-exclusion principle *)
let computeTotalPossibilities constraints =
    List.sum (List.map constraintPossibilitiesCount constraints)

let isAccepted statements variables =
    let env = { statements = statements; variables = variables }
    eval env (Map.find "in" statements)

let assignmentValue variables = Seq.sum (Map.values variables)

let part1 statements assignments =
    let acceptedAssignments = List.filter (isAccepted statements) assignments
    let result = List.sumBy assignmentValue acceptedAssignments
    printfn "part 1: %d" result

let part2 statements =
    let allConstraints = symbolicEval statements (Map.find "in" statements)
    let solvedConstraints = List.map solveConstraints allConstraints
    printfn "part 2: %d" (computeTotalPossibilities solvedConstraints)

let run lines =
    let (statements, assignments) = Utils.Parsing.runf pinput (String.concat "\n" lines)
    part1 statements assignments
    part2 statements

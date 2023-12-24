module Day22

open Utils

type Point3D = int * int * int
module Point3D =
    let ofString (s: string) =
        match s.Split ',' with
        | [|x; y; z|] -> (int x, int y, int z)
        | _ -> failwithf "could not parse string %s" s


type Cube3D = Point3D * Point3D
module Cube3D =
    let ofString (s: string) =
        match s.Split '~' with
        | [|s; e|] -> (Point3D.ofString s, Point3D.ofString e)
        | _ -> failwithf "could not parse string %s" s

    let intersect1d (sx1, ex1) (sx2, ex2) =
        sx1 <= ex2 && sx2 <= ex1

    let intersect ((sx1, sy1, _), (ex1, ey1, _)) ((sx2, sy2, _), (ex2, ey2, _)) =
        intersect1d (sx1, ex1) (sx2, ex2) && intersect1d (sy1, ey1) (sy2, ey2)

    let zStart ((_, _, sz), (_, _, ez)) = max sz ez
    let zEnd ((_, _, sz), (_, _, ez)) = min sz ez

    let moveDown ((sx, sy, sz), (ex, ey, ez)) = ((sx, sy, sz - 1), (ex, ey, ez - 1))

    let zs cube = { zEnd cube .. zStart cube }

let removeCubeFromMapping cube groupedCubes =
    let folder m z = Map.change z (Option.map (Set.remove cube)) m
    Seq.fold folder groupedCubes (Cube3D.zs cube)

let addCubeToMapping cube groupedCubes =
    let changer = function
    | Some v -> Some (Set.add cube v)
    | None -> Some (Set.ofList [cube])
    let folder m z = Map.change z changer m
    Seq.fold folder groupedCubes (Cube3D.zs cube)

let groupByZ cubes =
    List.fold (flip addCubeToMapping) Map.empty cubes

let getIntersectingAt z cube groupedCubes =
    let candidates = Map.tryFind z groupedCubes |> Option.defaultValue Set.empty
    let pred c = c <> cube && Cube3D.intersect c cube
    Set.filter pred candidates

let getSupporting cube groupedCubes =
    getIntersectingAt (Cube3D.zStart cube + 1) cube groupedCubes

let getSupporters cube groupedCubes =
    getIntersectingAt (Cube3D.zEnd cube - 1) cube groupedCubes

let getUniquelySupportedBy cube groupedCubes =
    let supporting = getSupporting cube groupedCubes
    Set.filter (fun c -> Set.count (getSupporters c groupedCubes) = 1) supporting

let isOnlySupportedBy cube cubes groupedCubes =
    let supporters = getSupporters cube groupedCubes
    Set.count (Set.difference supporters cubes) = 0

let moveCubeDown cube groupedCubes =
    let newGroupedCubes = removeCubeFromMapping cube groupedCubes
    let rec run current =
        let zEnd = Cube3D.zEnd current
        let candidates = Map.tryFind (zEnd - 1) newGroupedCubes |> Option.defaultValue Set.empty
        let pred c = Cube3D.intersect c current
        if zEnd = 1 || Set.exists pred candidates
            then current
            else run (Cube3D.moveDown current)
    let newCube = run cube
    addCubeToMapping newCube newGroupedCubes

let moveCubesDown cubes =
    let sortedCubes = List.sortBy Cube3D.zStart cubes
    let groupedCubes = groupByZ sortedCubes
    List.fold (flip moveCubeDown) groupedCubes sortedCubes

let countFallNumbers cube groupedMovedCubes =
    let rec run supported supporters count =
        let allSupported = Set.map (fun c -> getSupporting c groupedMovedCubes) supported |> Set.unionMany
        let newSupported = Set.filter (fun c -> isOnlySupportedBy c supporters groupedMovedCubes) allSupported
        if Set.isEmpty newSupported then count
        else run newSupported (Set.union supporters newSupported) (count + Set.count newSupported)
    let initialSet = Set.ofList [cube]
    run initialSet initialSet 0

let part1 groupedMovedCubes =
    let movedCubes = Seq.concat (Map.values groupedMovedCubes) |> Set.ofSeq |> Set.toList
    let pred c = Set.count (getUniquelySupportedBy c groupedMovedCubes) = 0
    let disintegratables = List.filter pred movedCubes
    printfn "part 1: %d" (List.length disintegratables)

let part2 groupedMovedCubes =
    let movedCubes = Seq.concat (Map.values groupedMovedCubes) |> Set.ofSeq |> Set.toList
    let result = List.map ((flip countFallNumbers) groupedMovedCubes) movedCubes
    printfn "part 2: %d" (List.sum result)

let run lines =
    let cubes = List.map Cube3D.ofString lines
    let movedCubes = moveCubesDown cubes
    part1 movedCubes
    part2 movedCubes

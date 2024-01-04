namespace Utils

type Grid2D<'a> = {
    map: Map<Point2D, 'a>;
    height: int64;
    width: int64;
}

module Grid2D =
    let empty: Grid2D<'a> = { map = Map.empty; height = 0L; width = 0L }
    let map f grid = { map = Map.map f grid.map; height = grid.height; width = grid.width }

    let neighbors includeDiagonals point { map = map } =
        let neighboringPoints = Point2D.neighbors includeDiagonals point
        let getValue p = Map.tryFind p map |> Option.map (fun v -> (p, v))
        List.choose getValue neighboringPoints

    let fromLines lines =
        let lineFolder y m (x, c) = Map.add (int64 x, int64 y) c m
        let folder m (y, line) = Seq.fold (lineFolder y) m (Seq.indexed line)
        let map = List.fold folder Map.empty (List.indexed lines)
        { map = map; height = Seq.length lines; width = Seq.length (List.head lines) }

    let getRow y { map = map; width = width } =
        let xIndices = { 0L..width - 1L }
        Seq.map (fun x -> Map.find (x, y) map) xIndices |> Seq.toArray

    let getCol x { map = map; height = height } =
        let yIndices = { 0L..height - 1L }
        Seq.map (fun y -> Map.find (x, y) map) yIndices |> Seq.toArray

    let allRows grid =
        let yIndices = { 0L..grid.height - 1L }
        Seq.map ((flip getRow) grid) yIndices |> Seq.toArray

    let allCols grid =
        let xIndices = { 0L..grid.width - 1L }
        Seq.map ((flip getCol) grid) xIndices |> Seq.toArray

    let toString emptyChar { map = map; width = width; height = height } =
        let getValue p = Map.tryFind p map |> Option.defaultValue emptyChar |> string
        let produceLine y = { 0L..width - 1L } |> Seq.map (fun x -> getValue (x, y)) |> String.concat " "
        { 0L..height - 1L } |> Seq.map produceLine |> String.concat "\n"

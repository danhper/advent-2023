namespace Utils

type Grid2D<'a> = Map<Point2D, 'a>

module Grid2D =
    let empty: Grid2D<'a> = Map.empty

    let neighbors point grid =
        let neighboringPoints = Point2D.neighbors point
        let getValue p = Map.tryFind p grid |> Option.map (fun v -> (p, v))
        List.choose getValue neighboringPoints

    let fromLines lines =
        let lineFolder y m (x, c) = Map.add (x, y) c m
        let folder m (y, line) = Seq.fold (lineFolder y) m (Seq.indexed line)
        List.fold folder Map.empty (List.indexed lines) 

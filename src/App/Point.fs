namespace Utils

type Point2D = int * int

module Point2D =
    let neighbors includeDiagonals ((x, y) as p) =
        if includeDiagonals then
            List.allPairs [x-1; x; x+1] [y-1; y; y+1]
            |> List.filter ((<>) p)
        else
            [(x - 1, y); (x + 1, y); (x, y - 1); (x, y + 1)]

    let manhattanDistance (x1, y1) (x2, y2) =
        let dx = if x1 > x2 then x1 - x2 else x2 - x1
        let dy = if y1 > y2 then y1 - y2 else y2 - y1
        dx + dy

    let manhattanDistanceU64 (x1: uint64, y1: uint64) (x2, y2) =
        let dx = if x1 > x2 then x1 - x2 else x2 - x1
        let dy = if y1 > y2 then y1 - y2 else y2 - y1
        dx + dy

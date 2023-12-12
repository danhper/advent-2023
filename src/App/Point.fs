namespace Utils

type Point2D = int * int

module Point2D =
    let neighbors includeDiagonals ((x, y) as p) =
        if includeDiagonals then
            List.allPairs [x-1; x; x+1] [y-1; y; y+1]
            |> List.filter ((<>) p)
        else
            [(x - 1, y); (x + 1, y); (x, y - 1); (x, y + 1)]

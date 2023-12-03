namespace Utils

type Point2D = int * int

module Point2D =
    let neighbors ((x, y) as p) =
        List.allPairs [x-1; x; x+1] [y-1; y; y+1]
        |> List.filter ((<>) p)

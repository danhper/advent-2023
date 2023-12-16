namespace Utils

type Direction = North | South | East | West
module Direction =
    let moveTo (x, y) direction =
        match direction with
        | North -> x, y - 1
        | South -> x, y + 1
        | East -> x + 1, y
        | West -> x - 1, y

    let inv = function
        | North -> South
        | South -> North
        | East -> West
        | West -> East

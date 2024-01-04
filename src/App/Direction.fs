namespace Utils

type Direction = North | South | East | West
module Direction =
    let all = [North; South; East; West]
    let moveToWithDelta (x, y) direction delta =
        match direction with
        | North -> x, y - delta
        | South -> x, y + delta
        | East -> x + delta, y
        | West -> x - delta, y

    let moveTo point direction = moveToWithDelta point direction 1L

    let inv = function
        | North -> South
        | South -> North
        | East -> West
        | West -> East

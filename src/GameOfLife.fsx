// https://en.wikipedia.org/wiki/Conway's_Game_of_Life

type Cell =
| Live
| Dead

let tryGetCell  (x:int) (y:int) (state:Cell array array) : Cell option =
    state
    |> Seq.tryItem x
    |> Option.map (Seq.tryItem y)
    |> Option.flatten

let getLiveNeighboursCount (x:int) (y:int) (state:Cell array array) : int =
    [   (x-1,y-1); (x,y-1); (x+1,y-1)
        (x-1,y);            (x+1,y)
        (x-1,y+1); (x,y+1); (x+1,y+1) ]
    |> Seq.map (fun (x,y) -> tryGetCell x y state)
    |> Seq.choose id
    |> Seq.filter ((=) Live)
    |> Seq.length

let cellTick (cell:Cell) (x:int) (y:int) (state:Cell array array) : Cell =
    match cell, getLiveNeighboursCount x y state with
    | Live, 2
    | Live, 3
    | Dead, 3 -> Live
    | _ -> Dead

let tick (state:Cell array array) : Cell array array =
    state
    |> Array.mapi (fun x cells ->
        cells
        |> Array.mapi (fun y cell ->
                cellTick cell x y state))
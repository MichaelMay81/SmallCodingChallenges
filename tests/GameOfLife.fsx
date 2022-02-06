#load "../src/GameOfLife.fsx"
open GameOfLife

let block ="----\n\
            -00-\n\
            -00-\n\
            ----\n"
let blinker1 ="-----\n\
               -----\n\
               -000-\n\
               -----\n\
               -----\n"
let blinker2 ="-----\n\
               --0--\n\
               --0--\n\
               --0--\n\
               -----\n"
let pulsar = "-----------------\n\
              -----------------\n\
              ----000---000----\n\
              -----------------\n\
              --0----0-0----0--\n\
              --0----0-0----0--\n\
              --0----0-0----0--\n\
              ----000---000----\n\
              -----------------\n\
              ----000---000----\n\
              --0----0-0----0--\n\
              --0----0-0----0--\n\
              --0----0-0----0--\n\
              -----------------\n\
              ----000---000----\n\
              -----------------\n\
              -----------------\n"

let stringToCells (str:string) =
    str.Split "\n"
    |> Seq.map 
        (Seq.map (function
            | '0' -> Live
            | '-' | _ -> Dead)
        >> Seq.toArray)
    |> Seq.toArray
    

let cellsToString =
    Array.map
        (Array.map (function | Dead -> "-" | Live -> "0")
        >> Array.fold (+) "")
    >> Array.fold (fun s1 s2 -> s1 + "\n" + s2) ""

printfn "results:"
block |> stringToCells |> tick |> cellsToString |> printfn "%s"
blinker1 |> stringToCells |> tick |> cellsToString |> printfn "%s"
blinker2 |> stringToCells |> tick |> cellsToString |> printfn "%s"

let rec repeat (times:int) (state:Cell array array) =
    match times with
    | 0 -> ()
    | _ ->
        System.Console.Clear ()
        let state = tick state
        state |> cellsToString |> printfn "%s"
        System.Threading.Thread.Sleep 500
        repeat (times-1) state

pulsar |> stringToCells |> repeat 300
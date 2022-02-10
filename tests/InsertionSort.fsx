#load "../src/InsertionSort.fsx"
#r "nuget: Unquote"

open InsertionSort
open Swensen.Unquote

// test
let rnd = System.Random()
let sampleSize = 50000
let rndList = List.init sampleSize (fun _ -> rnd.Next ())

printfn "InsertionSort\n"

printfn "Benchmark sample size: %i" sampleSize
printf "Builtin   "
#time "on"
let builtIn = rndList |> List.sort
#time "off"
printf "imut List "
#time "on"
let insertionSorted = rndList |> insertionSort
#time "off"
printf "mut Array "
#time "on"
let insertionSorted2 = rndList |> insertionSort2
#time "off"

printfn "Testing"
test <@ insertionSorted = builtIn @>
test <@ insertionSorted2 |> Array.toList = builtIn @>
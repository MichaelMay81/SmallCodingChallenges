#load "../src/QuickSort.fsx"
#r "nuget: Unquote"

open QuickSort
open Swensen.Unquote

// test
let rnd = System.Random()
let sampleSize = 50000
let rndList = List.init sampleSize (fun _ -> rnd.Next ())

printfn "QuickSort\n"

printfn "Benchmark sample size: %i" sampleSize
printf "Builtin       "
#time "on"
let builtIn = rndList |> List.sort
#time "off"
printf "dec imut List "
#time "on"
let quickSorted = rndList |> quickSort
#time "off"
printf "async         "
#time "on"
let quickSorted2 = rndList |> quickSort2
#time "off"

printfn "Testing"
test <@ quickSorted = builtIn @>
test <@ quickSorted2 = builtIn @>
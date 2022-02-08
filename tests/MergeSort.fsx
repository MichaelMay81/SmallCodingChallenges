#load "../src/MergeSort.fsx"
#r "nuget: Unquote"

open MergeSort
open Swensen.Unquote

// test
let rnd = System.Random()
let sampleSize = 50000
let rndList = List.init sampleSize (fun _ -> rnd.Next ())

printfn "MergeSort\n"

printfn "Benchmark sample size: %i" sampleSize
printf "Builtin       "
#time "on"
let builtIn = rndList |> List.sort
#time "off"
printf "dec imut List "
#time "on"
let mergeSorted = rndList |> mergeSort
#time "off"
printf "async         "
#time "on"
let mergeSorted2 = rndList |> mergeSort2
#time "off"

printfn "Testing"
test <@ mergeSorted = builtIn @>
test <@ mergeSorted2 = builtIn @>
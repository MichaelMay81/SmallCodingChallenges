#load "../src/BubbleSort.fsx"
#r "nuget: Unquote"

open BubbleSort
open Swensen.Unquote

// test
let rnd = System.Random()
let sampleSize = 5000
let rndList = List.init sampleSize (fun _ -> rnd.Next ())

printfn "BubbleSort\n"

printfn "Benchmark sample size: %i" sampleSize
printf "Builtin          "
#time "on"
let builtIn = rndList |> List.sort
#time "off"
printf "dec imut List    "
#time "on"
let bubbleSorted = rndList |> bubbleSort
#time "off"
printf "w/o last element "
#time "on"
let bubbleSorted2 = rndList |> bubbleSort2
#time "off"
printf "imp mut Array    "
#time "on"
let bubbleSorted3 = rndList |> bubbleSort3 |> Array.toList
#time "off"
printf "rec mut Array    "
#time "on"
let bubbleSorted4 = rndList |> bubbleSort4 |> Array.toList
#time "off"

printfn "Testing"
test <@ bubbleSorted = builtIn @>
test <@ bubbleSorted2 = builtIn @>
test <@ bubbleSorted3 = builtIn @>
test <@ bubbleSorted4 = builtIn @>
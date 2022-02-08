// https://en.wikipedia.org/wiki/Merge_sort

let rec private merge list1 list2 result =
    match list1, list2 with
    | [], [] -> result |> List.rev
    | [], _ -> (result |> List.rev) @ list2
    | _, [] -> (result |> List.rev) @ list1
    | e1::l1, e2::l2 ->
        if e1 < e2
        then merge l1 list2 (e1::result)
        else merge list1 l2 (e2::result)

let mergeSort (listToSort: 'A list) : 'A list =
    let rec mergeAll = function
        | [] -> []
        | [ l1 ] -> l1
        | lists ->
            lists
            |> List.chunkBySize 2
            |> List.map (function
                | [l1; l2] -> merge l1 l2 []
                | [l1] -> l1
                | _ -> [])
            |> mergeAll

    mergeAll (listToSort |> List.map List.singleton)

/// Async MergeSort (Overhead of Async makes this slower...)
let mergeSort2 (listToSort: 'A list) : 'A list =
    let rec mergeAll : 'A list list -> 'A list  = function
        | [] -> []
        | [ l1 ] -> l1
        | lists ->
            lists
            |> List.chunkBySize 2
            |> List.map (fun ll -> (async { return ll |> (
                function
                | [l1; l2] -> merge l1 l2 []
                | [l1] -> l1
                | _ -> [] )}))
            |> Async.Parallel
            |> Async.RunSynchronously
            |> Array.toList
            |> mergeAll

    mergeAll (listToSort |> List.map List.singleton)
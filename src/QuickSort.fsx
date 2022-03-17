// https://en.wikipedia.org/wiki/Quicksort

let quickSort (listToSort: 'A list) : 'A list =
    let pickAndRun list func =
        match list with
        | [] -> []
        | [el] -> [el]
        | head::tail -> func head tail
    let rec partition before after pivot list =
        match list with
        | [] ->
            let beforeSorted = (partition [] []) |> pickAndRun before
            let afterSorted = (partition [] []) |> pickAndRun after
            beforeSorted @ (pivot :: afterSorted)
        | head::tail ->
            if head < pivot
            then partition (head::before) after pivot tail
            else partition before (head::after) pivot tail
    
    (partition [] []) |> pickAndRun listToSort

/// async
// let quickSort2 (listToSort: 'A list) : 'A list =
//     let pickAndRun list func =
//         match list with
//         | [] -> []
//         | [el] -> [el]
//         | head::tail -> func head tail
//     let rec partition before after pivot list =
//         match list with
//         | [] ->
//             let beforeLength = before |> List.length
//             let afterLength = after |> List.length
//             let [| beforeSorted; afterSorted |] =
//                 [ before; after ]
//                 |> List.map (fun list -> async { return (partition [] []) |> pickAndRun list })
//                 // |> Async.Parallel
//                 |> (fun asyncs ->
//                         asyncs
//                         |>  if beforeLength > 100 && afterLength > 100
//                             then Async.Parallel
//                             else Async.Sequential)
//                 |> Async.RunSynchronously
//             beforeSorted @ (pivot :: afterSorted)
//         | head::tail ->
//             if head < pivot
//             then partition (head::before) after pivot tail
//             else partition before (head::after) pivot tail
    
//     (partition [] []) |> pickAndRun listToSort

let quickSort2 (listToSort: 'A list) : 'A list =
    let pickAndRun func list =
        match list with
        | [] -> []
        | [el] -> [el]
        | head::tail -> func head tail
    let rec partition before after pivot list =
        match list with
        | [] ->
            let beforeLength = before |> List.length
            let afterLength = after |> List.length
            let pickAndRun = pickAndRun (partition [] [])
            let [| beforeSorted; afterSorted |] =
                [| before; after |]
                |> if beforeLength > 30000 && afterLength > 30000
                    then
                        Array.map (fun list -> async { return pickAndRun list})
                        >> Async.Parallel
                        >> Async.RunSynchronously
                    else
                        Array.map pickAndRun
            beforeSorted @ (pivot :: afterSorted)
        | head::tail ->
            if head < pivot
            then partition (head::before) after pivot tail
            else partition before (head::after) pivot tail
    
    listToSort |> pickAndRun (partition [] [])
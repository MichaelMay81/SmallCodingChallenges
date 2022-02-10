// https://en.wikipedia.org/wiki/Insertion_sort

let insertionSort (listToSort: 'A list) : 'A list =
    let rec insert listOut listIn element =
        match listIn with
        | [] -> element::listOut |> List.rev
        | next::rest ->
            if element <= next
            then (element::listOut |> List.rev) @ listIn
            else insert (next::listOut) rest element
    let rec sort listOut listIn =
        match listIn with
        | [] -> listOut
        | next::rest ->
            sort (insert [] listOut next) rest
    sort [] listToSort

/// mutable array
let insertionSort2 (seqToSort: 'A seq) : 'A array =
    let rec insert (array:'A array) element id =
        if id = -1 || element >= array[id] then
            array[id+1] <- element
        else
            array[id+1] <- array[id]
            insert array element (id-1)
    
    let arrayToSort = seqToSort |> Seq.toArray
    [ 1 .. (arrayToSort |> Array.length) - 1 ]
    |> List.map (fun i -> insert arrayToSort arrayToSort[i] (i-1))
    |> ignore
    arrayToSort
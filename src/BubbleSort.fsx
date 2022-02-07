// https://en.wikipedia.org/wiki/Bubble_sort

let rec bubbleSort (listToSort: 'A list) : 'A list =
    let rec bs (listBegin:'A list) (listEnd:'A list) : 'A list =
        match listBegin, listEnd with
        | _, [] -> listBegin |> List.rev
        | [], next::le -> bs [next] le
        | first::lb, next::le ->
            if first > next
            then bs (first::next::lb) le
            else bs (next::first::lb) le
            
    let newList = bs [] listToSort
    if listToSort = newList
    then newList
    else (bubbleSort newList)

/// skip last element
let bubbleSort2 (listToSort: 'A list) : 'A list =
    let rec bs (listBegin:'A list) (listEnd:'A list) (listStart:'A list): 'A list =
        match listBegin, listEnd with
        | [], [] -> listStart |> List.rev
        | last::lb, [] ->
            if (listBegin |> List.rev) = listStart
            then listBegin
            else
                let lb = lb |> List.rev
                last :: (bs [] lb lb)
        | [], next::le -> bs [next] le listStart
        | first::lb, next::le ->
            if first > next
            then bs (first::next::lb) le listStart
            else bs (next::first::lb) le listStart
            
    bs [] listToSort listToSort |> List.rev

/// swap two elements in mutable array
let private swap (a:int) (b:int) (array:'A array) : unit =
    let value = array[a]
    array[a] <- array[b]
    array[b] <- value

/// imperative style with mutable array
let bubbleSort3 (listToSort:'A seq) : 'A array =
    let listToSort = listToSort |> Seq.toArray
    let mutable n = listToSort |> Seq.length
    while n > 1 do
        let mutable newn = 0
        for i = 1 to (n-1) do
            if listToSort[i-1] > listToSort[i] then
                listToSort |> swap (i-1) i
                newn <- i
        n <- newn
    listToSort

/// rec loop with mutable array (w/o other mutables)
let bubbleSort4 (listToSort:'A seq) : 'A array =
    let listToSort = listToSort |> Seq.toArray
    let rec loop n i newn =
        if n > 1 then
            if i < n then
                if listToSort[i-1] > listToSort[i] then
                    listToSort |> swap (i-1) i
                    loop n (i+1) i
                else loop n (i+1) newn
            else loop newn 1 0
    
    loop (listToSort |> Seq.length) 1 0
    listToSort
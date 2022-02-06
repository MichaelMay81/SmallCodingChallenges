// https://en.wikipedia.org/wiki/Fizz_buzz

let fizzbuzz : int seq -> string list =
  Seq.map (fun i ->
    match i % 3, i % 5 with
    | 0, 0 -> "Fizz Buzz"
    | _, 0 -> "Buzz"
    | 0, _ -> "Fizz"
    | _ -> string i)
  >> Seq.toList
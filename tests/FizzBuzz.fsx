#load "../src/FizzBuzz.fsx"
#r "nuget: Unquote"

open FizzBuzz
open Swensen.Unquote

let fb36 =
  {1..36}
  |> fizzbuzz
  |> List.fold (fun state str -> state + str + ", ") ""

printfn "fizzbuzz: %s" fb36

let result = "1, 2, Fizz, 4, Buzz, Fizz, 7, 8, Fizz, Buzz, 11, Fizz, 13, 14, Fizz Buzz, 16, 17, Fizz, 19, Buzz, Fizz, 22, 23, Fizz, Buzz, 26, Fizz, 28, 29, Fizz Buzz, 31, 32, Fizz, 34, Buzz, Fizz, " // ...
test <@ fb36 = result @>
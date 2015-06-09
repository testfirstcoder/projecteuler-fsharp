module ``Problem 25 - 1000 digit Fibonacci number``

let fibonacci =
    let rec fibonacci i a b = 
        seq { yield i, b; yield! fibonacci (i + 1) b (a + b) }

    fibonacci 1 0I 1I

let fibonacciNumberIndex length =
    fibonacci 
    |> Seq.find (fun (_, e) -> e |> string |> String.length |> (=) length) 
    |> fst

#if INTERACTIVE
#time
fibonacciNumberIndex 1000
#time
#endif

open NUnit.Framework
open Swensen.Unquote

[<TestCase(3, 12)>]
[<TestCase(1000, 4782)>]
let ``Index of the first Fibonacci term with n digits`` length result =
    test <@ fibonacciNumberIndex length = result @>
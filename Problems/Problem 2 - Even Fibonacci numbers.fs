module ``Problem 2 - Even Fibonacci numbers``

open NUnit.Framework
open Swensen.Unquote

let even n = n &&& 1 <> 1

let fibonacci = 
    let rec fibonacci (a, b) = seq { yield a; yield! fibonacci (b, b + a) }
    fibonacci(1, 2)

let ``Σ Fibonacci numbers < n`` predicate n = 
    fibonacci 
    |> Seq.takeWhile (fun e -> e <= n) 
    |> Seq.filter predicate
    |> Seq.sum

[<TestCase(2, 2)>]
[<TestCase(8, 10)>]
[<TestCase(100, 44)>]
[<TestCase(4000000, 4613732)>]
let ``Σ even Fibonacci numbers < n`` n sum =
    test <@ ``Σ Fibonacci numbers < n`` even n = sum @>
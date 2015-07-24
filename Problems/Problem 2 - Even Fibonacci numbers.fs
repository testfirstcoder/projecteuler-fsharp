module ``Problem 2 - Even Fibonacci numbers``

let even n = n &&& 1 <> 1

let fibonacci =
    let rec fibonacci (a, b) = seq { yield a; yield! fibonacci (b, b + a) }
    fibonacci (1, 2)

let ``Σ even Fibonacci numbers`` n =
    fibonacci
    |> Seq.takeWhile (fun e -> e <= n)
    |> Seq.filter even
    |> Seq.sum

#if INTERACTIVE
#time
``Σ even Fibonacci numbers`` 4000000
#time
#endif

open NUnit.Framework
open Swensen.Unquote

[<TestCase(2, 2)>]
[<TestCase(8, 10)>]
[<TestCase(100, 44)>]
[<TestCase(4000000, 4613732)>]
let ``Σ even Fibonacci numbers < n`` n sum =
    test <@ ``Σ even Fibonacci numbers`` n = sum @>
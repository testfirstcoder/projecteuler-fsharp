module ``Problem 1 - Multiples of 3 and 5``

open NUnit.Framework
open Swensen.Unquote

let ``Σ of multiples`` predicate n  = 
    [|1..n - 1|] 
    |> Array.filter predicate 
    |> Array.sum

let ``Σ of multiples of 3 or 5`` n = 
    ``Σ of multiples`` (fun x -> x % 3 = 0 || x % 5 = 0) n

[<TestCase(10, 23)>]
[<TestCase(1000, 233168)>]
let ``Σ of all the multiples of 3 or 5 for natural numbers < n`` n result =
    test <@ ``Σ of multiples of 3 or 5`` n = result @>
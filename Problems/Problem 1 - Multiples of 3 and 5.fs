module ``Problem 1 - Multiples of 3 and 5``

open NUnit.Framework
open Swensen.Unquote

let sumOfMultiples predicate n  = [|1..n - 1|] |> Array.filter predicate |> Array.sum

let ``sum of multiples of 3 or 5`` n = sumOfMultiples (fun x -> x % 3 = 0 || x % 5 = 0) n

[<TestCase(10, 23)>]
[<TestCase(1000, 233168)>]
let ``sum of all the multiples of 3 or 5 for natural numbers below n``(n: int, result: int) =
    test <@ ``sum of multiples of 3 or 5`` n = result @>
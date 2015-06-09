module ``Problem 30 - Digit fifth powers``

let ``Σ n | n = Σ powers of digits of n`` power =
    let pow n  = pown n power

    [|2..200000|]
    |> Array.map (fun n -> n, n |> string |> Array.ofSeq)
    |> Array.map (fun (n, a) -> n, a |> Array.map (string >> int >> pow) |> Array.sum)
    |> Array.filter (fun (n, s) -> n = s)
    |> Array.sumBy fst

#if INTERACTIVE
#time
``Σ n | n = Σ powers of digits of n`` 5
#time
#endif

open NUnit.Framework
open Swensen.Unquote

[<TestCase(4, 19316)>]
[<TestCase(5, 443839)>]
let ``Σ of all the numbers that can be written as the Σ of powers of their digits`` power result =
    test <@ ``Σ n | n = Σ powers of digits of n`` power = result @>
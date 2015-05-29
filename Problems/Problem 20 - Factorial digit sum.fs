module ``Problem 20 - Factorial digit sum``

let factorialDigitSum n =
    [|1..n|]
    |> Array.map (fun e -> bigint e) 
    |> Array.reduce (*) 
    |> (string >> Array.ofSeq) 
    |> Array.map (string >> int)
    |> Array.sum

#if INTERACTIVE
#time
factorialDigitSum 100
#time
#endif

open NUnit.Framework
open Swensen.Unquote

[<TestCase(10, 27)>]
[<TestCase(100, 648)>]
let ``Sum of the digits in the number n!`` n result =
    test <@ factorialDigitSum n = result @>
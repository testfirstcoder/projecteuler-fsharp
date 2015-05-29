module ``Problem 5 - Smallest multiple``

let smallestMultiple limit =
    let rec gcd a b = match a % b with | 0 -> b | r -> gcd b r
    let lcm a b = a * b / (gcd a b)
    [|1..limit - 1|] |> Array.reduce lcm

open NUnit.Framework
open Swensen.Unquote

[<TestCase(10, 2520)>]
[<TestCase(20, 232792560)>]
let ``The smallest number that can be divided by each of the numbers from 1 to n without any remainder`` n result =
    test <@ smallestMultiple n = result @>

#if INTERACTIVE
#time
smallestMultiple 20
#time
#endif
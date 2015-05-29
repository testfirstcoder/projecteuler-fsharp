module ``Problem 9 - Special Pythagorean triplet``

let productOfSpecialPythagoreanTriplet sum = 
    [1.0..sum] 
    |> List.collect (fun e1 -> [for e2 in [e1..sum] -> e1, e2])
    |> List.find (fun (e1, e2) -> e1 * e1 + e2 * e2 = (sum - e1 - e2) ** 2.0)
    |> (fun (e1, e2) -> e1 * e2 * (e1 * e1 + e2 * e2 |> sqrt))

open NUnit.Framework
open Swensen.Unquote

[<TestCase(1000, 31875000)>]
let ``Product a * b * c, a² + b² = c² and a + b + c = sum `` sum result =
    test <@ productOfSpecialPythagoreanTriplet sum = result @>

#if INTERACTIVE
#time
productOfSpecialPythagoreanTriplet 1000.0
#time
#endif
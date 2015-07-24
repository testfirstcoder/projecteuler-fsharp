module ``Problem 9 - Special Pythagorean triplet``

let productOfSpecialPythagoreanTriplet sum =
    [1..sum]
    |> List.collect (fun a -> [for b in [a..sum] -> a, b])
    |> List.find (fun (a, b) -> a * a + b * b = pown (sum - a - b) 2)
    |> (fun (a, b) -> a * b * (a * a + b * b |> float |> sqrt |> int))

#if INTERACTIVE
#time
productOfSpecialPythagoreanTriplet 1000
#time
#endif

open NUnit.Framework
open Swensen.Unquote

[<TestCase(1000, 31875000)>]
let ``Product a * b * c, a² + b² = c² and a + b + c = sum `` sum result =
    test <@ productOfSpecialPythagoreanTriplet sum = result @>
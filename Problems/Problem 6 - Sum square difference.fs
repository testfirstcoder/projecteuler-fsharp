module ``Problem 6 - Sum square difference``

let sumSquareDifference n =
    let square x = x * x
    let range = [|1..n|]

    range |> Array.map square |> Array.sum |> (-) (range |> Array.sum |> square)

#if INTERACTIVE
#time
sumSquareDifference 100
#time
#endif

open NUnit.Framework
open Swensen.Unquote

[<TestCase(10, 2640)>]
[<TestCase(100, 25164150)>]
let ``(1 + ... + n)² - (1² + ... + n²)`` n result =
    test <@ sumSquareDifference n = result @>
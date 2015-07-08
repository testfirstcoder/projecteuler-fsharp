module ``Problem 28 - Number spiral diagonals``

let ``Σ spiral diagonal numbers`` dimension =
    let rec ``Σ`` maxSpiralNumber shift =
        match maxSpiralNumber with
        | n when n = dimension * dimension -> 1
        | _ -> seq[for i in 1..4 -> i * shift + maxSpiralNumber] 
               |> (fun e -> Seq.sum e + ``Σ`` (Seq.last e) (shift + 2))
    
    ``Σ`` 1 2

#if INTERACTIVE
#time
``Σ spiral diagonal numbers`` 1001
#time
#endif

open NUnit.Framework
open Swensen.Unquote

[<TestCase(1, 1)>]
[<TestCase(3, 25)>]
[<TestCase(5, 101)>]
[<TestCase(1001, 669171001)>]
let ``Σ spiral diagonal numbers for odd dimensions`` dimension sum =
    test <@ ``Σ spiral diagonal numbers`` dimension = sum @>
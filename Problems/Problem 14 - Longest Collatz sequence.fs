module ``Problem 14 - Longest Collatz sequence``

let hash = new System.Collections.Generic.Dictionary<int64,int64>()
let store key value = hash.[key] <- value
                      value

let isPowerOfTwo n = n &&& (n - 1L) = 0L 
let log2 n = (float n, float 2) |> System.Math.Log |> int64
let term n = if n % 2L = 0L then n / 2L else 3L * n + 1L
   
let collatzSequenceLength n =
    let rec length n acc =
        match n with 
        | n when hash.ContainsKey n -> hash.[n]
        | n when n = 1L -> acc
        | n when isPowerOfTwo n -> log2(n) + 1L |> store n 
        | _ -> acc + 1L |> length (term n) |> (+) 1L |> store n

    length n 1L

let longestChain limit = 
    [|1L..limit|] 
    |> Array.mapi (fun i e -> i + 1, collatzSequenceLength e)
    |> Array.maxBy snd 
    |> fst

open NUnit.Framework
open Swensen.Unquote

[<TestCase(1, 1)>]
[<TestCase(2, 2)>]
[<TestCase(4, 3)>]
[<TestCase(3, 8)>]
[<TestCase(5, 6)>]
[<TestCase(13, 10)>]
let ``Σ Collatz sequence terms`` startingNumber length = test <@ collatzSequenceLength startingNumber = length @>

[<TestCase(1000000, 837799)>]
let ``Number with the longest chain`` limit result = test <@ longestChain limit = result @>

#if INTERACTIVE
#time
longestChain 1000000L
#time
#endif
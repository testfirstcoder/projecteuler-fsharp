module ``Problem 4 - Largest palindrome product``

let isPalindrome n = n |> string |> Array.ofSeq |> (fun e -> e = Array.rev e)

let largestPalindrome digitNumbersCount =
    let largestNumber = "9" |> String.replicate digitNumbersCount |> int
    
    [|1..largestNumber|] 
    |> Array.collect (fun e1 -> [|for e2 in e1..largestNumber -> e1 * e2|])
    |> Array.filter isPalindrome
    |> Array.max

open NUnit.Framework
open Swensen.Unquote

[<TestCase(2, 9009)>]
[<TestCase(3, 906609)>]
let ``Largest palindrome made from the product of two n-digit numbers`` digitNumbers result =
    test <@ largestPalindrome digitNumbers = result @>


#if INTERACTIVE
#time
largestPalindrome 3
#time
#endif
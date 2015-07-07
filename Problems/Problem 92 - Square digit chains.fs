module ``Problem 92 - Square digit chains``

let hash = System.Collections.Generic.Dictionary<int, int>()

let store key value = if hash.ContainsKey key = false then hash.Add(key,value)
                      value

let rec digitsSquare n = match n with | 0 -> 0 | _ -> pown (n % 10) 2 + digitsSquare (n / 10)
    
let numberChain n =
    let rec numberChain m =
        if hash.ContainsKey m then hash.[m]
        else match m with 
             | 1 | 89 -> m 
             | _ -> digitsSquare m |> numberChain |> store m

    numberChain n

let squareDigitChains n m =
    [|1..n|]
    |> Array.map numberChain
    |> Array.filter (fun e -> e = m)
    |> Array.length

#if INTERACTIVE
#time
squareDigitChains 10000000 89
#time
#endif
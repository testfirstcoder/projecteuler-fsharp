module ``Problem 48 - Self powers``

let selfPowers n = 
    [for i in 1..n -> pown (bigint i) i] 
    |> List.sum
    |> (fun e -> e % pown 10I 10)

#if INTERACTIVE
#time
selfPowers 1000
#time
#endif
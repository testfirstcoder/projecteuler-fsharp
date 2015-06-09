module ``Problem 16 - Power digit sum``

let powerDigitSum = pown 2I 1000 |> string |> Seq.map (string >> int) |> Seq.sum

#if INTERACTIVE
#time
powerDigitSum
#time
#endif
module ``Problem 56 - Powerful digit sum``

let powerfulDigitSum limit = 
    let bounds = [1..limit - 1]

    bounds 
    |> Seq.collect (fun a -> [for b in bounds -> pown (bigint a) b]) 
    |> Seq.distinct
    |> Seq.map (string >> Seq.map (string >> int) >> Seq.sum)
    |> Seq.max

#if INTERACTIVE
#time
powerfulDigitSum 100
#time
#endif
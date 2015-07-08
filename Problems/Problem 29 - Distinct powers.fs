module ``Problem 29 - Distinct powers``

let distinctPowers a b =
    [fst a..snd a] 
    |> Seq.collect (fun (a: int) -> [fst b..snd b] |> Seq.map (fun b -> pown (bigint a) b)) 
    |> Seq.distinct 
    |> Seq.length

#if INTERACTIVE
#time
distinctPowers (2,100) (2,100)
#time
#endif
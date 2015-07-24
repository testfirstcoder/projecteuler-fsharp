module ``Problem 40 - Champernowne's constant``

let champernowneConstant n  =
    let indexes = [|0..n|] |> Array.map (fun p -> pown 10 p - 1)

    [|1..Array.max indexes|]
    |> (Array.map string >> System.String.Concat)
    |> Array.ofSeq
    |> (fun a -> indexes |> Array.map (fun i -> a.[i]))
    |> Array.map (string >> int)
    |> Array.reduce (*)

#if INTERACTIVE
#time
champernowneConstant 6
#time
#endif
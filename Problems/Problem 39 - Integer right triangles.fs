module ``Problem 39 - Integer right triangles``

let sideLengths n = 
    [|1..n|]
    |> Array.collect (fun a -> [|for b in [|a..n|] -> a, b, a * a + b * b |> (float >> sqrt)|])
    |> Array.filter (fun (a,b,c) -> c % 1.0 = 0. && a + b + int c <= n) 
    |> Array.map (fun (a,b,c) -> a, b, int c)

let perimeterWithMaximisedNumberOfSolutions n =
    let sideLengths = sideLengths n

    [|1..n|]
    |> Array.map (fun p -> p, sideLengths |> Array.filter (fun (a,b,c) -> a + b + c = p))
    |> Array.maxBy (fun (a,b) -> Array.length b)
    |> fst

#if INTERACTIVE
#time
perimeterWithMaximisedNumberOfSolutions 1000
#time
#endif

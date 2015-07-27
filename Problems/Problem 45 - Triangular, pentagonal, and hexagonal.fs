module ``Problem 45 - Triangular, pentagonal, and hexagonal``

let sequence f = 
    let rec sequence n =  seq { yield f n; yield! n + 1L |> sequence }
    sequence 1L

let nextTriangleNumber lowerBound =
    let n = 100000

    [(fun n -> n * (n + 1L) / 2L) |> sequence |> Seq.skip lowerBound |> Seq.take n |> set
     (fun n -> n * (3L * n - 1L) / 2L) |> sequence |> Seq.take n |> set
     (fun n -> n * (2L * n - 1L)) |> sequence |> Seq.take n |> set]
    |> Set.intersectMany
    |> Set.minElement

#if INTERACTIVE
#time
nextTriangleNumber 285
#time
#endif
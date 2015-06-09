module ``Problem 44 - Pentagon numbers``

let pentagonNumbers = 
    let rec pentagonNumbers n = 
        seq { yield n * (3 * n - 1) / 2; yield! pentagonNumbers (n + 1) }
    
    pentagonNumbers 1

let difference limit =
    let numbers = System.Collections.Generic.HashSet<int>(pentagonNumbers |> Seq.take limit)
    let isPentagon n = numbers.Contains(n)
    let isPentagonPair a b = isPentagon(a + b) && isPentagon(b - a)
    
    numbers
    |> Seq.collect (fun e1 -> numbers 
                              |> Seq.filter(fun e2 -> e2 > e1 && (isPentagonPair e1 e2)) 
                              |> Seq.map(fun e2 -> e2 - e1))
    |> Seq.sort
    |> Seq.min

#if INTERACTIVE
#time
difference 5000
#time
#endif
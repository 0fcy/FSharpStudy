open System

let printHeader title =
    printfn "\n===== %s =====" title

let printSeq title (s: seq<'T>) (printer: 'T -> unit) =
    printHeader title
    s |> Seq.iter printer

let demoForVsIter () =
    let mySequence = seq { 1 .. 10 }

    printHeader "Creating a sequence and printing it (for vs Seq.iter)"
    printfn "Using a for loop:"
    for i in mySequence do
        printfn "  %d" i

    printfn ""
    printfn "Using Seq.iter:"
    mySequence |> Seq.iter (printfn "  %d")

let demoAllPairsAndAppend () =
    let a = seq { 1 .. 2 }
    let b = seq { 3 .. 5 }

    printHeader "Seq.allPairs - Cartesian product (tuples of all combinations)"
    Seq.allPairs a b
    |> Seq.iter (printfn "  %A")

    printHeader "Seq.append - Concatenate two sequences"
    Seq.append a b
    |> Seq.iter (printfn "  %d")

let demoAverage () =
    let c = seq { 1. .. 10. }   // sequence of floats
    printHeader "Seq.average - Average of a sequence of floats"
    c |> Seq.average |> printfn "  Average of c = %f"

    let d = seq { 2. .. 4. }
    printHeader "Seq.averageBy - Average computed from a projection of pairs"
    Seq.allPairs c d
    |> Seq.averageBy (fun (x, y) -> x + y)
    |> printfn "  Average of (c + d) pairs = %f"

let demoCache () =
    let uncachedSeq = seq {
        for i = 1 to 3 do
            printfn "  Calculating %i" i
            yield i
    }

    printHeader "Seq.cache - Cache side effects so each element is evaluated once"
    printfn "First iteration (will trigger calculation):"
    let cachedSeq = uncachedSeq |> Seq.cache
    cachedSeq |> Seq.iter (printfn "  %d")

    printfn "Second iteration (no recalculation):"
    cachedSeq |> Seq.iter (printfn "  %d")

let demoCast () =
    // Demonstrate casting from seq<obj> to a concrete type
    let objSeq = seq { box 1; box 2; box 3 }
    printHeader "Seq.cast - Cast a sequence of objects to a concrete type"
    objSeq
    |> Seq.cast<int>
    |> Seq.iter (printfn "  %d")

let demoChoose () =
    let integers = seq { 1 .. 10 }
    printHeader "Seq.choose - Filter and map using an option-returning function"
    integers
    |> Seq.choose (fun i ->
        if i % 2 = 0 then Some i else None)
    |> Seq.iter (printfn "  %d")

[<EntryPoint>]
let main _argv =
    demoForVsIter ()
    demoAllPairsAndAppend ()
    demoAverage ()
    demoCache ()
    demoCast ()
    demoChoose ()
    0


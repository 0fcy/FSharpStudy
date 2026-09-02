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

let demoChunkBySize () =
    let integers = seq { 1 .. 100 }
    printHeader "Seq.chunkBySize - Split a sequence to chunks of a specified size"
    integers
    |> Seq.chunkBySize 10
    |> Seq.iter (fun chunk ->
        chunk |> Seq.iter (printf "%d ")
        printfn "")

let demoChoose () =
    let integers = seq { 1 .. 10 }
    printHeader "Seq.choose - Filter and map using an option-returning function"
    integers
    |> Seq.choose (fun i ->
        if i % 2 = 0 then Some i else None)
    |> Seq.iter (printfn "  %d")
        
let demoCollect () =
    let integers = [1; 2; 3]
    printHeader "Seq.collect - Map to a collection, and flatten the result"
    integers
    |> Seq.collect (fun i -> seq { for j = 1 to 5 do yield i + j * 10 })
    |> Seq.iter (printfn "%d")

let demoCompareWith () =
    let first = seq { 1; 3; 5; 7; 9 }
    let second = seq { 1; 2; 3; 4; 5 }
    printHeader "Seq.compareWith - Compare each element of two seqences"
    
    let comparison = Seq.compareWith (fun i j -> if i > j then 0 else -1) first second
    let larger = comparison = 0
    printfn "Elements from first sequence are always larger?: %b" larger

let demoConcat () =
    let first = seq { 1 .. 2 .. 10 }
    let second = seq { 2 .. 2 .. 10 }
    let third = seq { 3 .. 2 .. 10 }
    printHeader "Seq.concat - Concat an enumeration of enumerations"

    let enumerations = seq { first; second; third }
    enumerations
    |> Seq.concat
    |> Seq.iter (printfn "%d")

let demoContains () =
    let integers = seq { 5 .. 15 }
    printHeader "Seq.contains - Check if a sequence contains an element"

    let contains = integers |> Seq.contains 10
    printfn "Sequence contains 10?: %b" contains

let demoCountBy () =
    let integers = seq {
        for i = 0 to 10 do
            for j = i to 10 do
                yield j
    }
    printHeader "Seq.countBy - Count the number of occurences of elements in a sequence"

    integers
    |> Seq.countBy (fun i -> i)
    |> Seq.iter (printfn "%O")

let demoDelay () =
    let myListFactory () = [
        for i in 1 .. 5 do
            printfn "Creating %d" i
            yield i
    ]
    printHeader "Seq.delay - Delay the evaluation of eager sequences (like lists)"

    let deferedSequence = 
        printfn "*Start creating defered sequence*"
        Seq.delay (fun () -> myListFactory () |> Seq.ofList)

    printfn "Start iteration:"
    deferedSequence |> Seq.iter (printfn "%d")

let demoDistinct () =
    let integers = seq {
        for i in 1 .. 10 do
            if i % 2 = 0 then
                yield i
            yield i
    }
    printHeader "Seq.distinct - Get distinct elements from a sequence"
    
    integers
    |> Seq.distinct
    |> Seq.iter (printfn "%d")

let demoDistinctBy () =
    let integers = seq { -5 .. 10 }
    printHeader "Seq.distinctBy - Get distinct elements from a sequence, providing a key function"
    
    integers
    |> Seq.distinctBy (fun i -> abs i)
    |> Seq.iter (printfn "%d")
    
let demoExactlyOne () =
    let integers = seq { 1 }
    printHeader "Seq.exactlyOne - Get the only element in a sequence (only when the sequence contains one element!)"
    
    integers
    |> Seq.exactlyOne
    |> printfn "%d"

let demoExcept () =
    let integers = seq { 1 .. 25 }
    printHeader "Seq.except - Exclude elements from a sequence"
    
    let odd = seq { for i in 1 .. 2 .. 25 do yield i }
    integers
    |> Seq.except odd
    |> Seq.iter (printfn "%d")

let demoExists () =
    let integers = seq { 0 .. 10 }
    printHeader "Seq.exists - Check if a sequence contains an element"
    
    let isPrime n =
        match n with
        | _ when n < 2 -> false
        | 2 -> true
        | _ when n % 2 = 0 -> false
        | _ ->
            seq { 3 .. 2 .. int (sqrt (float n))}
            |> Seq.forall (fun x -> n % x <> 0)

    integers
    |> Seq.exists isPrime
    |> printfn "Sequence up to ten contains prime numbers?: %b"

let demoExists2 () =
    let integers = seq { 1 .. 5 }
    let strings = seq { "one"; "two"; "three"; "four"; "five" }
    printHeader "Seq.exists2 - Check if pairs of elements from two sequences match a predicate"
    
    integers
    |> Seq.exists2 (fun (s: string) i -> i > s.Length) strings
    |> printfn "Any integer up to five is greater than the number of characters it it's word from?: %b"
    
let demoFilter () =
    let integers = seq { 1 .. 100 }
    printHeader "Seq.filter - Filter a sequence"
    
    let isTriangular n =
        let x = 8 * n + 1
        let sqrtX = int (sqrt (float x))
        sqrtX * sqrtX = x

    integers
    |> Seq.filter isTriangular
    |> Seq.iter (printfn "%d")

let demoFind () =
    let triangular = seq { for i in 1 .. 10 do i * (i + 1) / 2 }
    
    printHeader "Seq.find - Find the first element in a sequence that matches a predicate"
    triangular
    |> Seq.find (fun i -> i > 10)
    |> printfn "First triangular greater than 10: %d"

    printHeader "Seq.findBack - Find the first element from the end of a sequence that matches a predicate"
    triangular
    |> Seq.findBack (fun i -> i < 50)
    |> printfn "First triangular less than 50: %d"

    printHeader "Seq.findIndex - Find the first element in a sequence that matches a predicate, and return its index"
    triangular
    |> Seq.findIndex (fun i -> i > 10)
    |> printfn "Index of first triangular greater than 10: %d"

    printHeader "Seq.findIndexBack - Find the first element from the end of a sequence that matches a predicate, and return its index"
    triangular
    |> Seq.findIndexBack (fun i -> i < 50)
    |> printfn "Index of first triangular less than 50: %d"


let demoFold () =
    let integers = seq { 1 .. 10 }
    let folder state i = state + (sprintf " %d" i)

    printHeader "Seq.fold - Aggregate a sequence with a generic state type using a fold function"
    integers
    |> Seq.fold folder "Numbers from sequence:"
    |> printfn "%s"

let demoFoldBack () =
    let integers = seq { 1 .. 10 }
    let folder i state = state + (sprintf "%d " i)

    printHeader "Seq.foldBack - Aggregate a sequence with a generic state type using an inverted fold function"

    Seq.foldBack folder integers "Numbers from sequence backwards: "
    |> printfn "%s"


let demoFold2 () =
    let integers = seq { 1 .. 5 }
    let strings = seq { "one"; "two"; "three"; "four"; "five" }
    printHeader "Seq.fold2 - Aggregate two sequences with a generic state type using a fold function"

    let folder state i s = state + (sprintf "%i %s\n" i s)

    (integers, strings)
    ||> Seq.fold2 folder "Numbers from sequences:\n"
    |> printfn "%s"

let demoFoldBack2 () =
    let integers = seq { 1 .. 5 }
    let strings = seq { "one"; "two"; "three"; "four"; "five" }
    printHeader "Seq.foldBack2 - Aggregate two sequences with a generic state type using an inverted fold function"
    
    let folder i s state = state + (sprintf "\n%i %s" i s)
    
    (integers, strings)
    ||> Seq.foldBack2 folder
    <| "Number from two sequences backwards:"
    |> printfn "%s"

let demoForAll () =
    let integers = seq { 1 .. 10 }
    printHeader "Seq.forAll - Check if a predicate is true for all elements"

    integers
    |> Seq.forall (fun i -> i > 2)
    |> printfn "All elements are greater than two?: %b"
    
let demoForAll2 () =
    let integers = seq { 1 .. 5 }
    let strings = seq { "one"; "two"; "three"; "four"; "five" }
    printHeader "Seq.forAll2 - Check if all paris of elements from two sequences match a predicate"

    (integers, strings)
    ||> Seq.forall2 (fun i s -> i + s.Length > 3)
    |> printfn "Sum of integer and string.Length is more than three for all numbers up to 5?: %b"

[<EntryPoint>]
let main _argv =
    demoForVsIter ()
    demoAllPairsAndAppend ()
    demoAverage ()
    demoCache ()
    demoCast ()
    demoChunkBySize ()
    demoChoose ()
    demoCollect ()
    demoCompareWith ()
    demoConcat ()
    demoContains ()
    demoCountBy ()
    demoDelay ()
    demoDistinct ()
    demoDistinctBy ()
    demoExactlyOne ()
    demoExcept ()
    demoExists ()
    demoExists2 ()
    demoFilter ()
    demoFind ()
    demoFold ()
    demoFoldBack ()
    demoFold2 ()
    demoFoldBack2 ()
    demoForAll ()
    demoForAll2 ()
    0


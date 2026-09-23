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

let demoGroupBy () =
    let integers = seq { 1u .. 10u }
    printHeader "Seq.groupBy - Group elements in a sequence"

    let isPerfectSquare x =
        let s = uint (Math.Sqrt(float x))
        s * s = x

    let isFibonacci n =
        let val1 = 5u * n * n + 4u
        let val2 = 5u * n * n - 4u
        isPerfectSquare val1 || isPerfectSquare val2

    printfn "Is fibonacci?:"
    integers
    |> Seq.groupBy isFibonacci
    |> Seq.iter (printfn "%A")

let demoHead () =
    let integers = seq { 1 .. 10 }
    printHeader "Seq.head - Get the first element of the sequence"

    integers
    |> Seq.head
    |> printfn "First element was: %d"

let demoIndexed () =
    let strings = seq { "zero"; "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine"; "ten" }
    printHeader "Seq.indexed - Index each element in a sequence"

    strings
    |> Seq.indexed
    |> Seq.iter (printfn "%A")

let demoInit () =
    printHeader "Seq.init - Generate a sequence using a function"

    let mySequence i = i * (i + 1) / 2

    Seq.init 20 mySequence
    |> Seq.iter (printf "%d ")

let demoInitInfinite () =
    printHeader "Seq.initInfinite - Generate a sequence using a function without a bound"

    let mySequence i = i * (i + 1) / 2

    Seq.initInfinite mySequence
    |> Seq.takeWhile (fun i -> i < 500)
    |> Seq.iter (printf "%d ")

let demoInsertAt () =
    let integers = seq { 1 .. 10 }
    printHeader "Seq.insertAt - insert an element at an index in a sequence"

    integers
    |> Seq.insertAt 2 4
    |> Seq.iter (printfn "%d")

let demoInsertManyAt () =
    let integers = seq { 1 .. 10 }
    printHeader "Seq.insertManyAt - insert many elements at an index in a sequence"

    (seq { 5 .. 2 .. 10}, integers)
    ||> Seq.insertManyAt 4
    |> Seq.iter (printfn "%d")

let demoIsEmpty () =
    let integers = seq { 1 .. 10 }
    printHeader "Seq.isEmpty - check if a sequence is empty"

    integers
    |> Seq.isEmpty
    |> printfn "The sequence was empty?: %b"

let demoItem () =
    let integers = seq { 1 .. 10 }
    printHeader "Seq.item - get the element at the index"

    integers
    |> Seq.item 5
    |> printfn "Item at fifth index: %d"

let demoIter () =
    let even = seq { 0 .. 2 .. 20 }
    let triangular = Seq.init 10 (fun n -> n * (n + 1) / 2)
    printHeader "Seq.iter2 - iterate two sequences together"

    (even, triangular)
    ||> Seq.iter2 (printfn "First item: %d; Second item %d")
    
    let strings = seq { "zero"; "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine"; "ten" }
    printHeader "Seq.iteri - iterate with an index"

    strings
    |> Seq.iteri (fun i s -> printfn "%s is at index %d" s i)

    let otherStrings = seq { "zero"; "jeden"; "dwa"; "trzy"; "cztery"; "piec"; "szesc"; "siedem"; "osiem"; "dziewiec"; "dziesiec" }
    printHeader "Seq.iteri2 - iterate with an index on two sequences"

    (strings, otherStrings)
    ||> Seq.iteri2 (fun i e j -> printfn "%s ENG \t %s POL \t equals %d" e j i)

let demoLast () =
    let integers = seq { 0 .. 10 }
    printHeader "Seq.last - Get the last element in the sequence"

    integers
    |> Seq.last
    |> printfn "Last element was: %d"

let demoLength () =
    let triangular = Seq.initInfinite (fun n -> n * (n + 1) / 2)
    printHeader "Seq.length - Get the length of the sequence"

    triangular
    |> Seq.takeWhile (fun i -> i < 500)
    |> Seq.length
    |> printfn "Number of triangular numbers under 500: %d"

let demoMap () =
    let integers = seq { 1 .. 10 }
    printHeader "Seq.map - Map elements in a sequence using a function"

    integers
    |> Seq.map (fun i -> i * i)
    |> Seq.iter (printfn "%d")

let demoMap2 () =
    let odd = seq { 1 .. 2 .. 10 }
    let even = seq { 0 .. 2 .. 10 }
    printHeader "Seq.map2 - Map elements in two sequences using a function"

    (odd, even)
    ||> Seq.map2 (fun o e -> o * e)
    |> Seq.iter (printfn "%d")

let demoMap3 () =
    let odd = seq { 1 .. 2 .. 10 }
    let even = seq { 0 .. 2 .. 10 }
    let fib = Seq.unfold (fun (a, b) -> Some(a + b, (b, a + b))) (0, 1)
    printHeader "Seq.map3 - Map elements in three sequences using a function"

    (odd, even, fib)
    |||> Seq.map3 (fun o e f -> o * e * f)
    |> Seq.iter (printfn "%d")

let demoMapFold () =    
    let integers = seq { 1  .. 10 }
    printHeader "Seq.mapFold - Map elements in a sequence and apply Seq.fold"

    integers
    |> Seq.mapFold (fun s i -> (i * i, s + i * i)) 0
    |> printfn "%A"

let demoMapFoldBack () =
    let integers = seq { 1 .. 10 }
    printHeader "Seq.mapFoldBack - Map elements in a sequence and apply Seq.foldBack"

    Seq.mapFoldBack (fun i s -> (i * i, s + sprintf "%d " (i * i))) integers "Squares backwards from 10: "
    |> printfn "%A"

let demoMapi () =
    let strings = seq { "zero"; "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine"; "ten" }
    printHeader "Seq.mapi - Map elements in a sequence with an index"

    strings
    |> Seq.mapi (sprintf "equal to %d\t%s")
    |> Seq.iter (printfn "%s")

let demoMapi2 () =
    let strings = seq { "zero"; "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine"; "ten" }
    let otherStrings = seq { "zero"; "jeden"; "dwa"; "trzy"; "cztery"; "piec"; "szesc"; "siedem"; "osiem"; "dziewiec"; "dziesiec" }
    printHeader "Seq.mapi - Map elements in a sequence with an index"

    (strings, otherStrings)
    ||> Seq.mapi2 (sprintf "equal to %d\t%s\t%s")
    |> Seq.iter (printfn "%s")

let demoMax () =
    let integers = Seq.init 10 (fun _ -> Random.Shared.Next())
    printHeader "Seq.max - Get the largest element where by T's comparison"

    integers
    |> Seq.max
    |> printfn "Largest from ten random: %d"

let demoMaxBy () =
    let integers = Seq.init 10 (fun _ -> Random.Shared.Next())
    printHeader "Seq.maxBy - Get the largest element with a comparison function"

    integers
    |> Seq.maxBy (fun i -> i % 2 = 0)
    |> printfn "Largest even from ten random: %d" // Could be odd if no even numbers generated

let demoMin () =
    let integers = Seq.init 10 (fun _ -> Random.Shared.Next())
    printHeader "Seq.min - Get the smallest element where by T's comparison"

    integers
    |> Seq.min
    |> printfn "Smallest from ten random: %d"

let demoMinBy () =
    let integers = Seq.init 10 (fun _ -> Random.Shared.Next())
    printHeader "Seq.minBy - Get the smallest element with a comparison function"

    integers
    |> Seq.minBy (fun i -> i % 2 = 1)
    |> printfn "Smallest odd from ten random: %d"

let demoOfArray () =
    let integers = [|1 .. 10|]
    printHeader "Seq.ofArray - Use sequence from an array"

    integers
    |> Seq.ofArray
    |> Seq.iter (printfn "%d")

let demoOfList () =
    let integers = [1 .. 10]
    printHeader "Seq.ofList - Use sequence from a list"

    integers
    |> Seq.ofList
    |> Seq.iter (printfn "%d")

let demoPairwise () =
    let integers = seq { 1 .. 10 }
    printHeader "Seq.pairwise - Pair each element with it's previous element (except the first element)"

    integers
    |> Seq.pairwise
    |> Seq.iter (printfn "%A")

let demoPermute () =
    let integers = seq { 1 .. 10 }
    printHeader "Seq.permute - Provide a function to map indexes of elements"

    integers
    |> Seq.permute (fun i -> if i % 2 = 1 then i - 1 else i + 1)
    |> Seq.iter (printfn "%d")

let demoPick () =
    let integers = seq { 1 .. 10 }
    printHeader "Seq.permute - Provide a function to map indexes of elements"

    integers
    |> Seq.pick (fun i -> if sqrt (float i) > 2 then Some i else None)
    |> printfn "First value whose sqrt is greater than 2: %d"

let demoRandomChoice () =
    let integers = seq { 1 .. 10 }
    printHeader "Seq.randomChoice - Get a random element in the sequence"

    integers
    |> Seq.randomChoice
    |> printfn "Random choice: %d"

let demoRandomChoiceBy () =
    let integers = seq { 1 .. 10 }
    printHeader "Seq.randomChoiceBy - Get a random element in the sequence using a randomizer function"

    integers
    |> Seq.randomChoiceBy (fun () -> Random.Shared.NextDouble() ** 2)
    |> printfn "Random choice, skewed by n * n: %d"

let demoRandomChoices () =
    let integers = seq { 1 .. 10 }
    printHeader "Seq.randomChoices - Get random elements in the sequence"

    integers
    |> Seq.randomChoices 3
    |> Seq.iter (printfn "Three random choices: %d")

let demoRandomChoicesBy () =
    let integers = seq { 1 .. 10 }
    printHeader "Seq.randomChoicesBy - Get random elements in the sequence using a randomizer function"

    integers
    |> Seq.randomChoicesBy (fun () -> Math.Sin(Random.Shared.NextDouble() * Math.PI)) 3
    |> Seq.iter (printfn "Three random choices: %d")

type private ExcludeFirstFiveRandom() =
    inherit Random()

    override this.Next (minValue: int, maxValue: int): int = 
        if maxValue > 5 then
            base.Next(minValue + 5, maxValue)
        else
            base.Next(minValue, maxValue)

let demoRandomChoicesWith () =
    let integers = seq { 1 .. 10 }
    let random = new ExcludeFirstFiveRandom()
    printHeader "Seq.randomChoicesWith - Get random elements in the sequence using a random instance"
    
    integers
    |> Seq.randomChoicesWith random 3
    |> Seq.iter (printfn "Three random choices: %d")

let demoRandomChoiceWith () =
    let integers = seq { 1 .. 10 }
    let random = new ExcludeFirstFiveRandom()
    printHeader "Seq.randomChoiceWith - Get a random element in the sequence using a random instance"
    
    integers
    |> Seq.randomChoiceWith random
    |> printfn "Random choice: %d"

let demoRandomSample () =
    let integers = seq { 1 .. 10 }
    printHeader "Seq.randomSample - Get distinct random elements in a sequence"
    
    integers
    |> Seq.randomSample 5
    |> Seq.iter (printfn "%d")

let demoRandomSampleBy () =
    let integers = seq { 1 .. 10 }
    printHeader "Seq.randomSampleBy - Get distinct random elements in a sequence, using a randomizer function"
    
    integers
    |> Seq.randomSampleBy (fun () -> 1. - (Random.Shared.NextDouble() ** 2)) 5
    |> Seq.iter (printfn "%d")

let demoRandomSampleWith () =
    let integers = seq { 1 .. 10 }
    let random = new ExcludeFirstFiveRandom()
    printHeader "Seq.randomSampleWith - Get distinct random elements in a sequence, using a Random instance"
    
    integers
    |> Seq.randomSampleWith random 5
    |> Seq.iter (printfn "%d")

let demoRandomShuffle () =
    let integers = seq { 1 .. 10 }
    printHeader "Seq.randomShuffle - Randomly shuffle the order of the entire sequence"
    
    integers
    |> Seq.randomShuffle
    |> Seq.iter (printfn "%d")

let demoRandomShuffleBy () =
    let integers = seq { 1 .. 10 }
    printHeader "Seq.randomShuffleBy - Randomly shuffle the order of the entire sequence, with a randomizer function"
    
    integers
    |> Seq.randomShuffleBy (fun () -> 1. - (Random.Shared.NextDouble() ** 2))
    |> Seq.iter (printfn "%d")

let demoRandomShuffleWith () =
    let integers = seq { 1 .. 10 }
    let random = Random.Shared
    printHeader "Seq.randomShuffleWith - Randomly shuffle the order of the entire sequence, using a Random instance"
    
    integers
    |> Seq.randomShuffleWith random
    |> Seq.iter (printfn "%d")

let demoReadonly () = 
    let integers = [| 1 .. 10 |]
    printHeader "Seq.readonly - Create a readonly view of the sequence"
    
    let view =
        integers
        |> Seq.readonly

    try
        view
        :?> int array
        |> Seq.iter (printfn "%d")
    with
    | ex -> printfn "Cannot insert into readonly seq:\n%s" ex.Message

let demoReduce () =
    let triangular = seq { for i in 1 .. 10 do i * (i + 1) / 2 }
    printHeader "Seq.reduce - combine all elements of a sequence to a single value"
    
    let expression =
        triangular
        |> Seq.map string
        |> String.concat " + "

    triangular
    |> Seq.reduce (+)
    |> printfn "Result of %s = %d" expression

let demoReduceBack () =
    let triangular = seq { for i in 1 .. 10 do i * (i + 1) / 2 }
    printHeader "Seq.reduceBack - combine all elements of a sequence to a single value backwards"
    
    let expression =
        triangular
        |> Seq.rev
        |> Seq.map string
        |> String.concat " - "

    triangular
    |> Seq.reduceBack (fun x y -> y - x)
    |> printfn "Result of %s = %d" expression

let demoRemoveManyAt () =
    let integers = seq { 1 .. 10 }
    printHeader "Seq.removeManyAt - remove many elements in the sequence"

    integers
    |> Seq.removeManyAt 3 4
    |> Seq.iter (printfn "%d")

let demoReplicate () =
    printHeader "Seq.replicate - create a sequence with the same item up to a count"

    Seq.replicate 12 3
    |> Seq.iter (printfn "%d")

let demoRev () =
    let integers = seq { -10 .. 2 .. 10 }
    printHeader "Seq.rev - reverse the sequence"

    integers
    |> Seq.rev
    |> Seq.iter (printfn "%d")

let demoScan () =
    let integers = seq { -10 .. 10 }
    printHeader "Seq.scan - fold and create results for each intermediate element, stored in a sequence"

    integers
    |> Seq.scan (+) 0
    |> Seq.iter (printfn "%d")

let demoScanBack () =
    let integers = seq { -10 .. 10 }
    printHeader "Seq.scanBack - fold and create results for each intermediate element in reverse order, stored in a sequence"

    (integers, 0)
    ||> Seq.scanBack (+)
    |> Seq.iter (printfn "%d")

let demoSingleton () =
    printHeader "Seq.singleton - wrap a single element in a new sequence"

    10
    |> Seq.singleton
    |> printfn "%A"

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
    demoGroupBy ()
    demoHead ()
    demoIndexed ()
    demoInit ()
    demoInitInfinite ()
    demoInsertAt ()
    demoInsertManyAt ()
    demoIsEmpty ()
    demoItem ()
    demoIter ()
    demoLast ()
    demoLength ()
    demoMap ()
    demoMap2 ()
    demoMap3 ()
    demoMapFold ()
    demoMapFoldBack ()
    demoMapi ()
    demoMapi2 ()
    demoMax ()
    demoMaxBy ()
    demoMin ()
    demoMinBy ()
    demoOfArray ()
    demoOfList ()
    demoPairwise ()
    demoPermute ()
    demoPick ()
    demoRandomChoice ()
    demoRandomChoiceBy ()
    demoRandomChoices ()
    demoRandomChoicesBy ()
    demoRandomChoicesWith ()
    demoRandomChoiceWith ()
    demoRandomSample ()
    demoRandomSampleBy ()
    demoRandomSampleWith ()
    demoRandomShuffle ()
    demoRandomShuffleBy ()
    demoRandomShuffleWith ()
    demoReadonly ()
    demoReduce ()
    demoReduceBack ()
    demoRemoveManyAt ()
    demoReplicate ()
    demoRev ()
    demoScan ()
    demoScanBack ()
    demoSingleton ()
    0


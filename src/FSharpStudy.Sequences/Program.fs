
let mySequence = seq { 1 .. 3 .. 10 }

// Print sequence with for loop
for i in mySequence do
    printfn "%d" i

printfn ""

// Print sequence with Seq.iter
mySequence
    |> Seq.iter (printfn "%d")
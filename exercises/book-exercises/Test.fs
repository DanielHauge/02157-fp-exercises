namespace book_exercises

open Lecture1
open utility.helper

module Test =
    let Lecture1 =
        printfn "Test lecture1-1-fact: %s" (test (Lecture1.factsum 3) 6)
        printfn "Test lecture1-2-fact: %s" (test (Lecture1.factsum 5) 15)
        printfn "Test lecture1-3-fact: %s" (test (Lecture1.factsum 9) 45)

        printfn "Test lecture1-1-weirdsum: %s" (test (Lecture1.weirdsum 5 4) 35)
        printfn "Test lecture1-2-weirdsum: %s" (test (Lecture1.weirdsum 10 4) 60)
        printfn "Test lecture1-3-weirdsum: %s" (test (Lecture1.weirdsum 10 6) 91)

        printfn "Test lecture1-1-binomial: %s" (test (Lecture1.binomialCof 2 1) 2)
        printfn "Test lecture1-2-binomial: %s" (test (Lecture1.binomialCof 4 2) 6)

        printfn "Test lecture1-1-multiplicity: %s" (test (Lecture1.multiplicity 5 [5;5;5]) 3)
        printfn "Test lecture1-2-multiplicity: %s" (test (Lecture1.multiplicity 2 [2;3;4;5]) 1)
        printfn "Test lecture1-3-multiplicity: %s" (test (Lecture1.multiplicity 4 []) 0)
        printfn "Test lecture1-4-multiplicity: %s" (test (Lecture1.multiplicity 15 [15;15;15;15;15;15]) 6)
        printfn "Test lecture1-5-multiplicity: %s" (test (Lecture1.multiplicity 4 [4;5;4;5;2;3;4]) 3)








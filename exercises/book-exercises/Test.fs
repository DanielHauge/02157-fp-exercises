namespace book_exercises

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

    let Lecture2 =
        printfn "Test lecture2-1-f: %s" (test (Lecture2.f 24) true)
        printfn "Test lecture2-2-f: %s" (test (Lecture2.f 27) true)
        printfn "Test lecture2-3-f: %s" (test (Lecture2.f 29) false)
        printfn "Test lecture2-1-stringPow: %s" (test (Lecture2.stringPow "Hello " 2) "Hello Hello ")
        printfn "Test lecture2-2-stringPow: %s" (test (Lecture2.stringPow " yep #5 1234 " 5) " yep #5 1234  yep #5 1234  yep #5 1234  yep #5 1234  yep #5 1234 ")
        printfn "Test lecture2-1-evenN: %s" (test (Lecture2.evenN 2) [2;0])
        printfn "Test lecture2-2-evenN: %s" (test (Lecture2.evenN 5) [8;6;4;2;0])
        printfn "Test lecture2-3-evenN: %s" (test (Lecture2.evenN 0) [])
        printfn "Test lecture2-1-split: %s" (test (Lecture2.split [1;2;3;4;5;6;7;8;9;10]) ([1;3;5;7;9] , [2;4;6;8;10]))
        printfn "Test lecture2-2-split: %s" (test (Lecture2.split [5;32;65;34;123;42;76;32]) ([5;65;123;76] , [32;34;42;32]))
        printfn "Test lecture2-1-zip: %s" (test (Lecture2.zip [1;2;3;4] [1;2;3;4]) [(1,1);(2,2);(3,3);(4,4)])
        printfn "Test lecture2-1-predicateSum: %s" (test (Lecture2.predicateSum (fun x -> x<5) [1;2;3;4]) 4)
        printfn "Test lecture2-2-predicateSum: %s" (test (Lecture2.predicateSum (fun x -> x<3) [-4;-3;3;2;15;0;-15]) 5)



    let BookExerciseTests =
        Lecture2











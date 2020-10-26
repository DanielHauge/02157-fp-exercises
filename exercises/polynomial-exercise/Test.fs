namespace polynomial_exercise

open utility.helper
open polynomial

module Test =

    let AddTests =
        printfn "Test Add: %s" (test (add [1;2] [3;4;5;6]) [4;6;5;6])

    let MulCTests = 
        printfn "Test MulC: %s" (test (mulC 2 [2; 0; 0; 1]) [4; 0; 0; 2])

    let SubTests =
        printfn "Test Sub: %s" (test (sub [1;2] [3;4;5;6]) [-2;-2;-5;-6])

    let MulXTests = 
        printfn "Test MulX: %s" (test (mulX [2;0;0;1]) [0;2;0;0;1])

    let MulTests = 
        printfn "Test Mul: %s" (test (mul [2;3;0;1] [1;2;3]) [2;7;12;10;2;3])

    let EvalTests = 
        printfn "Test Eval: %s" (test (eval 2 [2;3;0;1]) 16)

    let IsLegalTests =
        printfn "IsLegal 1: %s" (test (isLegal [2;0;0;4]) true)
        printfn "IsLegal 2: %s" (test (isLegal [2;0;0;4;0]) false)
        printfn "IsLegal 3: %s" (test (isLegal [0;0;0;0;5]) true)
        printfn "IsLegal 4: %s" (test (isLegal []) true)

    let PruneTests =
        printfn "Prune 1: %s" (test (prune [2;0;0;4]) [2;0;0;4])
        printfn "Prune 2: %s" (test (prune [0;0;0;4;0;0]) [0;0;0;4])

    let ToStringTest =
        printfn "ToString Test for [2;3;4] :  %s" (toString [2;3;4])
        printfn "ToString Test for [-2;-3;4] :  %s" (toString [-2;-3;4])
        printfn "ToString Test for [-2;-3;4;-25;52] :  %s" (toString [-2;-3;4;-25;52])



    let DerivativeTest =
        printfn "Prune 2: %s" (test (derivative [2;2;2;2]) [2;4;6])

    let ComposeTests =
        printfn "Compose test: %s" (test (compose [2;0;0;4] [0;3;2]) [2; 0; 0; 108; 216; 144; 32])

    let DegreeTEsts =
        printfn "Degree test 1: %s" (test (deg [2;0;0;4]) (Degree.FinN 3))
        printfn "Degree test 2: %s" (test (deg [2;0;0;4;5;5;5]) (Degree.FinN 6))
        printfn "Degree test 3: %s" (test (deg [2]) (Degree.FinN 0))
        printfn "Degree test 4: %s" (test (deg []) (Degree.MinusInf))
        printfn "Degree equality test 1: %s" (test (deg [] < deg [2]) (true))
        printfn "Degree equality test 2: %s" (test (deg [5;5;5] < deg [2;0;0;3]) (true))
        printfn "Degree add test 2: %s" (test (addD (deg ([5;5;5]),(deg [2;0;0;3]))) (Degree.FinN 5))
        printfn "Degree add test 2: %s" (test (addD (deg ([]),(deg [2;0;0;3]))) (Degree.MinusInf))
           






    let AllPolynomialTests =
        AddTests
        MulCTests
        SubTests
        MulXTests
        MulTests
        EvalTests
        ComposeTests
        DegreeTEsts


    
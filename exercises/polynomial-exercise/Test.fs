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

    let AllPart1Tests =
        AddTests
        MulCTests
        SubTests
        MulXTests
        MulTests
        EvalTests
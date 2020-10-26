namespace polynomial_exercise

open utility.helper
open polynomial_exercise.polynomial

module test =

    let AddTests =
        printfn "Test Add: %s" (test (add (ofList [1;2]) (ofList[3;4;5;6])) (ofList [4;6;5;6]))

    let MulCTests = 
        printfn "Test MulC: %s" (test (mulC 2 (ofList [2; 0; 0; 1]) ) (ofList [4; 0; 0; 2]))

    let SubTests =
        printfn "Test Sub: %s" (test (sub (ofList [1;2]) (ofList [3;4;5;6])) (ofList [-2;-2;-5;-6]))

    let MulXTests = 
        printfn "Test MulX: %s" (test (mulX (ofList [2;0;0;1])) (ofList [0;2;0;0;1]))

    let MulTests = 
        printfn "Test Mul: %s" (test (mul (ofList [2;3;0;1]) (ofList [1;2;3])) (ofList [2;7;12;10;2;3]))

    let EvalTests = 
        printfn "Test Eval: %s" (test (eval 2 (ofList [2;3;0;1])) 16)

    //let IsLegalTests =
    //    printfn "IsLegal 1: %s" (test (isLegal [2;0;0;4]) true)
    //    printfn "IsLegal 2: %s" (test (isLegal [2;0;0;4;0]) false)
    //    printfn "IsLegal 3: %s" (test (isLegal [0;0;0;0;5]) true)
    //    printfn "IsLegal 4: %s" (test (isLegal []) true)

    //let PruneTests =
    //    printfn "Prune 1: %s" (test (prune [2;0;0;4]) [2;0;0;4])
    //    printfn "Prune 2: %s" (test (prune [0;0;0;4;0;0]) [0;0;0;4])

    //let ToStringTest =
    //    printfn "ToString Test for [2;3;4] :  %s" (toString [2;3;4])
    //    printfn "ToString Test for [-2;-3;4] :  %s" (toString [-2;-3;4])
    //    printfn "ToString Test for [-2;-3;4;-25;52] :  %s" (toString [-2;-3;4;-25;52])



    //let DerivativeTest =
    //    printfn "Prune 2: %s" (test (derivative [2;2;2;2]) [2;4;6])

    //let ComposeTests =
    //    printfn "Compose test: %s" (test (compose [2;0;0;4] [0;3;2]) [2; 0; 0; 108; 216; 144; 32])

    //let DegreeTEsts =
    //    printfn "Degree test 1: %s" (test (deg [2;0;0;4]) (Degree.FinN 3))
    //    printfn "Degree test 2: %s" (test (deg [2;0;0;4;5;5;5]) (Degree.FinN 6))
    //    printfn "Degree test 3: %s" (test (deg [2]) (Degree.FinN 0))
    //    printfn "Degree test 4: %s" (test (deg []) (Degree.MinusInf))
    //    printfn "Degree equality test 1: %s" (test (deg [] < deg [2]) (true))
    //    printfn "Degree equality test 2: %s" (test (deg [5;5;5] < deg [2;0;0;3]) (true))
    //    printfn "Degree add test 2: %s" (test (addD (deg ([5;5;5]),(deg [2;0;0;3]))) (Degree.FinN 5))
    //    printfn "Degree add test 2: %s" (test (addD (deg ([]),(deg [2;0;0;3]))) (Degree.MinusInf))
           


    let AllPolynomialTests =
        AddTests
        MulCTests
        SubTests
        MulXTests
        MulTests
        EvalTests
        // ComposeTests
        // DegreeTEsts


    //// Part 5
    //let addInvSimple p1 p2 = if isLegal p1 && isLegal p2 then isLegal(add p1 p2) else true
    //let addInv p1 p2 = isLegal(add (prune p1) (prune p2))
    //let pruneInv p1 = isLegal(prune p1)
    //let mulCInv p1 k = isLegal (mulC k (prune p1))
    //let mulInv p1 p2 = isLegal(mul (prune p1) (prune p2))

    let polynomialEquality p1 p2 = List.forall (fun (a,b) -> a = b) (List.zip p1 p2)
    
    //let associative func p1 p2 p3 = polynomialEquality (func (func (prune p1) (prune p2)) (prune p3)) (func (prune p1) (func (prune p2) (prune p3)))

    //let associativeAdd p1 p2 p3 = associative add p1 p2 p3
    //let associativeMul p1 p2 p3 = associative mul p1 p2 p3
    //let associativeComp p1 p2 p3 = associative compose p1 p2 p3

    //let commutativeInv p1 p2 = polynomialEquality (add (prune p1) (prune p2)) (add (prune p2) (prune p1))
    //let additiveIdentity p1 = polynomialEquality (add (prune p1) []) (prune p1) && polynomialEquality (prune p1) (add [] (prune p1)) 
    //let additiveInverse p1 = polynomialEquality (add (prune p1) (mulC -1 (prune p1) )) []
    //let commutativeMultiply p1 p2 = polynomialEquality (mul (prune p1) (prune p2)) (mul (prune p2) (prune p1))
    //let multiplyIdentity p1 = polynomialEquality (mul (prune p1) [1]) (prune p1) && polynomialEquality (prune p1) (mul [1] (prune p1)) 
    //let distributive p1 p2 p3 = polynomialEquality (mul (prune p1) (add (prune p2) (prune p3))) (add (mul (prune p1) (prune p2)) (mul (prune p1) (prune p3)))

    //let evalAddTest k p1 p2 = eval k (add (prune p1) (prune p2)) = eval k (prune p1) + eval k (prune p2)
    //let evalMultiTest k p1 p2 = eval k (mul (prune p1) (prune p2)) = eval k (prune p1) * eval k (prune p2)


    //let degreeTest1 p1 p2 = deg (add (prune p1) (prune p2)) <= max (deg p1) (deg p2)
    //let degreeTest2 p1 p2 = deg (mul (prune p1) (prune p2)) <= addD ((deg p1),(deg p2))



    //let PropertyBasedTesting =
    //    FsCheck.Check.Quick addInvSimple
    //    FsCheck.Check.Quick addInv
    //    FsCheck.Check.Quick pruneInv
    //    FsCheck.Check.Quick mulCInv
    //    FsCheck.Check.Quick mulInv
    //    FsCheck.Check.Quick associativeAdd
    //    FsCheck.Check.Quick associativeMul
    //    FsCheck.Check.Quick commutativeInv
    //    FsCheck.Check.Quick additiveIdentity
    //    FsCheck.Check.Quick additiveInverse
    //    FsCheck.Check.Quick commutativeMultiply
    //    FsCheck.Check.Quick multiplyIdentity
    //    FsCheck.Check.Quick distributive
    //    FsCheck.Check.Quick evalAddTest
    //    FsCheck.Check.Quick evalMultiTest
    //    FsCheck.Check.Quick degreeTest1
    //    FsCheck.Check.Quick degreeTest2










        

    
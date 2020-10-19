namespace misc_exercises

open misc_exercises.listops
open utility.helper
module listops_test =
    let memberOfTest_1 = printfn "Test memberOf-1: %s" (test (memberOf(5,[1;2;3;4;5;6])) true)
    let memberOfTest_2 = printfn "Test memberOf-2: %s" (test (memberOf(0,[0;1;2;3])) true)
    let memberOfTest_3 = printfn "Test memberOf-3: %s" (test (memberOf(25,[2;3])) false)
    let memberOfTest_4 = printfn "Test memberOf-4: %s" (test (memberOf(5,[])) false)
    let memberOfTest_5 = printfn "Test memberOf-5: %s" (test (memberOf(5,[5])) true)

    let MemberOfTests =
        memberOfTest_1
        memberOfTest_2
        memberOfTest_3
        memberOfTest_4
        memberOfTest_5


    let insertTest_1 = printfn "Test insert-1: %s" (test_l (insert 5 [1;2;3;4;6]) [1;2;3;4;5;6])
    let insertTest_2 = printfn "Test insert-2: %s" (test_l (insert 7 []) [7])
    let insertTest_3 = printfn "Test insert-3: %s" (test_l (insert 2 [1;2;3;4;6]) [1;2;2;3;4;6])
    let insertTest_4 = printfn "Test insert-4: %s" (test_l (insert 12552 [1;2;3;4;6]) [1;2;3;4;6;12552])
    let insertTest_5 = printfn "Test insert-5: %s" (test_l (insert -5 [1;2;3;4;6]) [-5;1;2;3;4;6])

    let InsertTests =
        insertTest_1
        insertTest_2
        insertTest_3
        insertTest_4
        insertTest_5
        

    let sortTest_1 = printfn "Test sort-1: %s" (test_l (sort [1;7;0;2;9;4]) [0;1;2;4;7;9])
    let sortTest_2 = printfn "Test sort-2: %s" (test_l (sort [2;0;1;6;7;8;4;0]) [0;0;1;2;4;6;7;8])
    let sortTest_3 = printfn "Test sort-3: %s" (test_l (sort [1;2;3;4;5;6]) [1;2;3;4;5;6])
    let sortTest_4 = printfn "Test sort-4: %s" (test_l (sort [5;-5;25]) [-5;5;25])
    let sortTest_5 = printfn "Test sort-5: %s" (test_l (sort [9;8;7;6;5;4;3;2;1]) [1;2;3;4;5;6;7;8;9])

    let sortTests = 
        sortTest_1
        sortTest_2
        sortTest_3
        sortTest_4
        sortTest_5


    let Tests =
        MemberOfTests
        
        InsertTests

        sortTests
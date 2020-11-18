// Learn more about F# at http://fsharp.org

open book_exercises
open polynomial_exercise
open compiler_exercise

[<EntryPoint>]
let main argv =
    printfn "Running excersise tests"

    // Exercise lecture 1
   // listops_test.Tests
    
    // Exercises from book
    // Test.BookExerciseTests
    
    // Polynomial Excersise
    //Test.AllPolynomialTests
    // Test.PropertyBasedTesting

    // Company club exercise:
    //simple_company_club_exercise.club.test

    Compiler.testExecution
    printf "%i" ((3+7)+(4-5))

    0 // return an integer exit code
// Learn more about F# at http://fsharp.org

open book_exercises
open System
open polynomial_exercise
open simple_company_club_exercise

[<EntryPoint>]
let main argv =
    printfn "Running excersise tests"

    // Exercise lecture 1
   // listops_test.Tests
    
    // Exercises from book
    // Test.BookExerciseTests
    
    // Polynomial Excersise
    Test.AllPolynomialTests

    // Company club exercise:
    //simple_company_club_exercise.club.test

    0 // return an integer exit code
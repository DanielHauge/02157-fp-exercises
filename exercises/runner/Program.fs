// Learn more about F# at http://fsharp.org

open book_exercises
open polynomial_exercise

[<EntryPoint>]
let main argv =
    printfn "Running excersise tests"

    // Exercise lecture 1
   // listops_test.Tests
    
    // Exercises from book
    Test.BookExerciseTests

    // Polynomial Excersise
    // Test.AllPart1Tests

    0 // return an integer exit code
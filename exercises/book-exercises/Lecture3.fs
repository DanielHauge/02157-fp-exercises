module Lecture3
// Exercises in book:  Functional Programming using F#, Michael R. Hansen and Hans Rischel, Cambridge University Press, 2013
// 3.3

// The set of complex numbers is the set of pairs of real numbers. Complex numbers behave almost
// like real numbers if addition and multiplication are defined by:
// (a, b) + (c, d) = (a + c, b + d)
// (a, b) · (c, d) = (ac − bd, bc + ad)
// 1. Declare suitable infix functions for addition and multiplication of complex numbers.
// 2. The inverse of (a, b) with regard to addition, that is, −(a, b), is (−a,−b), and the inverse of
// (a, b) with regard to multiplication, that is, 1/(a, b), is (a/(a^2+b^2),−b/(a^2+b^2)) (provided
// that a and b are not both zero). Declare infix functions for subtraction and division of complex
// numbers.
// 3. Use let-expressions in the declaration of the division of complex numbers in order to avoid
// repeated evaluation of identical subexpressions.


type Complex = 
    { 
        real : int 
        imaginary : int
    }

    static member (+) (a : Complex, b : Complex) = { real = a.real+b.real; imaginary = a.imaginary+b.imaginary }
    static member (-) (a : Complex, b : Complex) = { real = a.real-b.real; imaginary = a.imaginary-b.imaginary }
    static member (*) (a : Complex, b : Complex) = { real = a.real*b.real - a.imaginary*b.imaginary; imaginary = a.imaginary*b.real + a.real*b.imaginary }





// HR 4.18 Pencil exercise
// f -> g -> a' list
// Hvor g er en higher order function. Den applier g på alle i listen.


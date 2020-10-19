module Lecture2
// Exercises in book:  Functional Programming using F#, Michael R. Hansen and Hans Rischel, Cambridge University Press, 2013
// 2.1, 2.2, 2.13, 4.3, 4.8, 4.9, 4.12

// 2.1 Declare a function f: int -> bool such that f(n) = true exactly when n is divisible by 2
// or divisible by 3 but not divisible by 5. Write down the expected values of f(24), f(27), f(29)
// and f(30) and compare with the result. Hint: n is divisible by q when n%q = 0.




// 2.2 Declare an F# function pow: string * int -> string, where:
//pow(s, n) = s * s * .....  s | n times
// where we use · to denote string concatenation. (The F# representation is +.)




// 2.13 The functions curry and uncurry of types
// curry : (’a * ’b -> ’c) -> ’a -> ’b -> ’c
// uncurry : (’a -> ’b -> ’c) -> ’a * ’b -> ’c
// are defined in the following way:
// curry f is the function g where g x is the function h where h y = f(x, y).
// uncurry g is the function f where f(x, y) is the value h y for the function h = g x.
// Write declarations of curry and uncurry.




// 4.3 Declare function evenN: int -> int list such that evenN n generates the list of the first
// n non-negative even numbers.




//4.8 Declare an F# function split such that:
//split [x0;x1;x2;x3; . . . ;xn−1] = ([x0;x2; . . . ], [x1;x3; . . . ])


//4.9 Declare an F# function zip such that:
//zip([x0;x1; . . . ;xn−1],[y0;y1; . . . ;yn−1])
//= [(x0, y0);(x1, y1); . . . ;(xn−1, yn−1)]
//The function should raise an exception if the two lists are not of equal length.


//4.12 Declare a function sum(p, xs) where p is a predicate of type int -> bool and xs is a list of
//integers. The value of sum(p, xs) is the sum of the elements in xs satisfying the predicate p.
//Test the function on different predicates (e.g., p(x) = x > 0).
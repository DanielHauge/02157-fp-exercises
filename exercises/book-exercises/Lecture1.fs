module Lecture1
// Exercises in book:  Functional Programming using F#, Michael R. Hansen and Hans Rischel, Cambridge University Press, 2013
// 1.4, 1.6, 2.8, 4.7

    //1.4 Declare a recursive function f: int -> int, where
    //f(n) = 1+2+· · · + (n − 1) + n
    //for n ≥ 0. (Hint: use two clauses with 0 and n as patterns.)
    //State the recursion formula corresponding to the declaration.
    //Give an evaluation for f(4).

    let rec factsum n = 
        match n with
        | 0 -> n
        | _ -> n + (factsum (n-1))



//1.6 Declare a recursive function sum: int * int -> int, where
//sum(m, n) = m + (m + 1) + (m + 2) + · · · + (m + (n − 1)) + (m + n)
//for m ≥ 0 and n ≥ 0. (Hint: use two clauses with (m,0) and (m,n) as patterns.)
//Give the recursion formula corresponding to the declaration.

    let rec weirdsum m n =
        match (m, n) with
        | (m, 0) -> m
        | (m, n) -> (m + n) + weirdsum m (n-1)


// 2.8 The following figure gives the first part of Pascal’s triangle:
//          1
//         1 1
//        1 2 1
//       1 3 3 1
//      1 4 6 4 1
// The entries of the triangle are called binomial coefficients. The k’th binomial coefficient of the
// n’th row is denoted <n, k> for n ≥ 0 and n ≥ k ≥ 0. for example <2,1>=2 and <4,2>=6 The first
// and last binomial coefficients, that is, <n,0> and <n,n> of row n and both 1. A binomial coefficient
// inside a row is the sum of the two binomial coefficients immediately above it. These properties
// can be expressed as follows. 
// <n,0> = <n,n> = 1
// and 
// <n,k>=<n-1,k-1>+<n-1,k> if n != 0, k != 0 and n > KeyValue
// Declare an F# function bin: int * int -> int to compute binomial coefficients.

    let rec binomialCof n k =
        match (n,k) with
        | (n,0) -> binomialCof n n
        | (n,k) when n=k -> 1
        | (n,k) -> (binomialCof (n-1) (k-1)) + (binomialCof (n-1) k)


// 4.7 Declare an F# function multiplicity x xs to find the number of times the value x occurs in the list xs.
    let rec multiplicity x xs =
        match xs with
        | [] -> 0
        | head::tail when head = x -> 1 + multiplicity x tail
        | _::tail -> multiplicity x tail

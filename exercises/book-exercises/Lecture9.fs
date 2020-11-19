module Lecture9
// Exercises in book:  Functional Programming using F#, Michael R. Hansen and Hans Rischel, Cambridge University Press, 2013
// 9.3 Declare an iterative solution to exercise 1.6 
// 1.6
// Declare a recursive function sum: int * int -> int, where
// sum(m, n) = m + (m + 1) + (m + 2) + · · · + (m + (n − 1)) + (m + n)
// for m ≥ 0 and n ≥ 0. (Hint: use two clauses with (m,0) and (m,n) as patterns.)
// Give the recursion formula corresponding to the declaration

// 9.4 Give iterative declarations of the list function List.length


// 9.3 
let rec weirdsum m n =
    match (m, n) with
    | (m, 0) -> m
    | (m, n) -> (m + n) + weirdsum m (n-1)

let rec weirdsum_inter m n a =
    match (m,n) with
    | (m, 0) -> m
    | (m, n) -> weirdsum_inter (m+a) (n-1) (n + a)




// 9.4
let rec listLength l a =
    match l with
    | _::tail -> listLength tail (a+1)
    | [] -> a


let test =
    printfn "%i" (weirdsum 5 5)
    printfn "%i" (weirdsum_inter 5 5 0)
    printfn "%i" (listLength [1;2;3;4] 0)
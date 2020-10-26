module Lecture4
// Exercises in book:  Functional Programming using F#, Michael R. Hansen and Hans Rischel, Cambridge University Press, 2013
// 5.3 Solve Exercise 4.12 using List.fold or List.foldBack.

// 4.12 Declare a function sum(p, xs) where p is a predicate of type int -> bool and xs is a list of
// integers. The value of sum(p, xs) is the sum of the elements in xs satisfying the predicate p.
// Test the function on different predicates (e.g., p(x) = x > 0).

// 4.12
let rec sum p xs =
    match xs with
    | [] -> 0
    | head::tail when p head -> head+sum p tail
    | _::tail -> sum p tail

// 5.3
let sumFold p xs = List.fold (fun a b -> a+b) 0 (List.filter p xs) 
let sumFoldBack p xs = List.foldBack (fun a b -> a+b) (List.filter p xs) 0
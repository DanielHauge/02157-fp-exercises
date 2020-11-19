module Week9
     
    let rec f x =
        function
        | [] -> []
        | y::ys -> (x,y)::f x ys


    let rec allPairs xs ys =
        match xs with
        | [] -> []
        | x::xrest -> f x ys @ allPairs xrest ys


    // 1:
    // for f
    // Input is a list of some sort which can be infered from the patterns of the function body.
    // It can be seen that the return value is a list of tuples of (x,y) as it returns the concatination of (x,y) with the applied f x tail.
    // x is a generic type as it is just used in any special kind of way, its most general type is a' and the inputs elements are not used in any special way either, so it is also a generic type which might be different from x, so it can be b'.

    // for allPairs:
    // xs can be infered to be a list of a' by the pattern matching.
    // ys can be infered to be a generic list also, as it is used as argument in f where it is a list of a generic type.
    // output is a list also infered from first pattern -> [], but can be infered as a list of tuples with 2 potentially different generic types as the elements of the list is the result of f. 
    // and f's output is a tuple of 2 potentially different types.

    // 2:
    // f "a" [1;2;3]
    // ("a",1)::f "a" [2;3]
    // ("a",1)::("a",2)::f "a" [3]
    // ("a",1)::("a",2)::("a", 3)::f "a" [] 
    // ("a",1)::("a",2)::("a", 3)::[] // *This is where there is no longer any recursive calls, as base case has been reached, and returns start.
    // ("a",1)::("a",2)::[("a", 3)]
    // ("a",1)::[("a",2);("a", 3)]
    // [("a",1);("a",2);("a", 3)]

    // 3: as f = a' -> b' list -> (a' * b') list. ie. "a" is a string, and [1;2;3] is an int list. Therefor replacing the generic types a and b with actual exeuction types, it is (string*int) list

    // 4: It is very apperent in the way exeuctions are shown in question 2. 
    // For each recursive call that occurs, a pending list concatination operation is added to the call stack.
    // Meaning that from the *comment it has to go back concatinating previous results to the list. Where as a tail recursive function would have accumulated the results to the list in its parameters. 
    // ie. The last recursive call could be something like: f "a" [] [("a",1);("a",2);("a", 3)] -> where it would reach basecase and just return the accumulated list immedietly.

    // 5
    let rec ftailrec x a =
        function
        | [] -> []
        | y::ys -> ftailrec x ((x,y)::a) ys 


    // 6
    let flist x s = List.map (fun e -> (x,e)) s

    let execute =
        printf "hejsa"
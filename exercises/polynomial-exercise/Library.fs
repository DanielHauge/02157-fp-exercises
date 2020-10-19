namespace polynomial_exercise


module polynomial =
    
    let rec add a b =
        match (a,b) with
        | ([],[]) -> []
        | ([],_) -> b
        | (_,[]) -> a
        | (aHead::aTail, bHead::bTail) -> aHead+bHead::add aTail bTail

    let rec mulC k a = 
        match a with
        | [] -> []
        | _ when k = 0 -> []
        | head::tail -> head*k::mulC k tail

    let sub a b = add a (mulC -1 b)

    let mulX a = 0::a

    let rec mul a b = 
        match (a,b) with
        | ([],_) -> []
        | (_,[]) -> []
        | (aH::aT,_) -> add (mulC aH b) (mulX (mul aT b))

    let rec pow x n = 
        match (x,n) with 
        | (_,0) -> 1
        | (x,n) -> x * pow x (n-1)

    let rec reverse xs = 
        match xs with
        | [] -> []
        | [_] -> xs
        | head::tail -> reverse tail @ [head]

    let eval x a = 
        let rec eval_rec ar =
             match ar with
            | [] -> 0
            | head::tail ->  (eval_rec tail) + (head * (pow x (ar.Length-1)))
        eval_rec (reverse a)
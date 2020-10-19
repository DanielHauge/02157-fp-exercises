namespace polynomial_exercise


module polynomial =
    
    // Part 1

    // type Poly = int list

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

    // Part 2

    let rec isLegal xs = 
        match xs with
        | [] -> true
        | [last] -> last <> 0 
        | _::tail -> isLegal tail

    let prune xs = 
        let rec prune_rec l =
            match l with
            | head::tail when head = 0 -> prune_rec tail
            | _ -> l

        reverse (prune_rec (reverse xs))

    let rec toString p = 

        let formatLed a n = 
            match n with
            | 0 -> sprintf "%i" (abs a)
            | 1 -> sprintf "%ix" (abs a)
            | _ -> sprintf "%ix^%i" (abs a) n

        let firstElements li =
                match li with 
                | head::_ -> head
                | _ -> 0
                
        
        let rec toString_rec p n =
            match p with
            | head::tail when head = 0 -> "0" + formatLed head n + toString_rec tail (n+1)
            | head::tail when head > 0 -> " + " + formatLed head n + toString_rec tail (n+1)
            | head::tail when head < 0 -> " - " + formatLed head n + toString_rec tail (n+1)
            | [last] when last > 0 -> " + " + formatLed last n
            | [last] when last < 0 -> " - " + formatLed last n
            | [] -> ""

        if (firstElements p) > 0 then (toString_rec p 0).[2..] else (toString_rec p 0)


    let derivative p =
        let rec derivative_rec p n =
            match p with
            | head::tail -> head*n::derivative_rec tail (n+1)
            | [last] -> [last*n]
            | [] -> []

        (derivative_rec p 0).[1..]


    let compose a b =

        let rec p_pow p n =
            match (p,n) with 
            | (_,-1) -> p
            | (_,0) -> p
            | (p,n) -> mul p (p_pow p (n-1))

        let rec compose_rec a b n =
            match a with
            | head::tail when n = 0 -> head::(compose_rec tail b (n+1)).[1..]
            | head::tail -> add (mulC head (p_pow b (n-1))) (compose_rec tail b (n+1))
            | [last] -> p_pow (mulC last b) n
            | [] -> []

        compose_rec a b 0

    
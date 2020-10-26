namespace polynomial_exercise


module polynomial =
    
    
    // Part 3 (Refactoring functions to preserve invariant.
    // Invariant of something: eg. preserves the invariant of "IsLegal" meaning, that when inputs to a function IsLegal, if invarient is preserved the output also has IsLegal properties. It remains unchanged.

    // Part 1
    type Poly = int list
    type Degree = 
        | MinusInf
        | FinN of int


    let deg p = 
        match List.length p with
        | 0 -> MinusInf
        | l -> Degree.FinN (l-1)

    let addD = 
        function
        | (Degree.MinusInf,_) -> Degree.MinusInf
        | (_,Degree.MinusInf) ->  Degree.MinusInf
        | (Degree.FinN aDegree ,Degree.FinN bDegree) -> Degree.FinN (aDegree+bDegree)


    let rec reverse xs = 
        match xs with
        | [] -> []
        | [_] -> xs
        | head::tail -> reverse tail @ [head]

    let prune xs : Poly = 
        let rec prune_rec l =
            match l with
            | head::tail when head = 0 -> prune_rec tail
            | _ -> l

        reverse (prune_rec (reverse xs))

    // P3: polynomial degrees can cancel each other out if b contains the inverse at a degree. Pruning to ensure IsLegal property even after add.
    let add a b =
        let rec add_rec a b =
            match (a,b) with
            | ([],[]) -> []
            | ([],_) -> b
            | (_,[]) -> a
            | (aHead::aTail, bHead::bTail) -> aHead+bHead::add_rec aTail bTail
            | _ -> []
        prune (add_rec a b)

    // P3: Multiply by anything other than 0 gives a new Polynomoial that is still IsLegal.
    let rec mulC k a : Poly = 
        match a with
        | [] -> []
        | _ when k = 0 -> []
        | head::tail -> head*k::mulC k tail

    // P3: Function is using add which preserves invariant IsLegal, hence this does to.
    let sub a b : Poly = add a (mulC -1 b)

    // P3: This preserves invariant IsLegal, as it does not modify last element.
    let mulX a : Poly = 0::a

    // P3: This preserveds invariant IsLegal, as it uses add,mulC and mulX which all preserves invariant IsLegal, this does too.
    let rec mul a b : Poly = 
        match (a,b) with
        | ([],_) -> []
        | (_,[]) -> []
        | (aH::aT,_) -> add (mulC aH b) (mulX (mul aT b))

    let rec pow x n = 
        match (x,n) with 
        | (_,0) -> 1
        | (x,n) -> x * pow x (n-1)

    
    // P3: Does not output a poly, hence cannot preserve invariant IsLegal.
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

    
    // P3: Does not output a poly, hence cannot preserve invariant IsLegal.
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

    // P3: This does not make the last element 0. As the only element that gets multiplied by 0 is the first element, and that is then skipped. This preserves the invariant IsLegal
    let derivative p : Poly =
        let rec derivative_rec p n =
            match p with
            | head::tail -> head*n::derivative_rec tail (n+1)
            | [] -> []

        (derivative_rec p 0).[1..]

    // P3: Uses functions that preserves the invaraint IsLegal, this does too.
    let compose a b : Poly =

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

   
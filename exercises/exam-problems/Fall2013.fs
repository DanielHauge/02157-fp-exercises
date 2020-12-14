module Fall2013

module Fall2013Q1 =

    
    // Problem 1 (q 1-5)
    type Multiset<'a when 'a : equality> = ('a * int) list
    
    let rec duplicates xs =
        match xs with
        | [] -> false
        | head::tail when List.contains head tail -> true
        | _::tail -> duplicates tail

    // 1
    let inv = 
        function 
        | gg -> (List.forall (function (a,b) -> b > 0) gg) && not (duplicates (List.map (fun (a,b) -> a) gg))

    // 2
    let insert a n xs =
        if n < 1 then xs else
        let exisistingIndex = List.tryFindIndex (function (xsa,_) -> xsa = a) xs
        if exisistingIndex.IsSome then xs.GetSlice(Some 0, Some exisistingIndex.Value) @ [(a, n)] @ xs.GetSlice(Some exisistingIndex.Value, Some (xs.Length-1)) else [(a, n)] @ xs

    // 3 
    let numberOf e ms = 
        let elem = List.tryFind (fun (a, _) -> a=e) ms
        if elem.IsSome then snd elem.Value else 0

    // 4
    let delete e ms = List.map (fun (a, o) -> if a=e then (a,(o-1)) else (a,o)) ms

    // 5
    let rec union xs ys = 
        match xs with
        | [] -> ys
        | (a,n)::tail ->  insert a n (union tail ys)



module Fall2013Q2 =
    // f given a list of integers computes a list of tuples with the element and an accumulating integer that gets powered by 2.
    // f : int -> 'a list -> (int * 'a) list
    let rec f i = function 
                | [] -> []
                | x::xs -> (i,x)::f (i*i) xs

    // g computes the first branch that satisfies predicate. Using DFS approach.
    // g : ('a -> bool) -> 'a Tree -> 'a Tree Option
    type 'a Tree = | LF
                   | Br of 'a Tree * 'a * 'a Tree

    let rec g p = function
            | LF -> None
            | Br(_,a,t) when p a -> Some t
            | Br(t1,a,t2) -> match g p t1 with
                             | None -> g p t2
                             | res -> res


    let rec f_trec i acc = function
                    | [] -> acc
                    | x::xs -> f_trec (i*i) ((i,x)::acc) xs

    let rec f_contrec i = function
    | [] -> []
    | x::xs -> let contFunc = f_contrec (i*i) xs
               (x,i)::contFunc

module Fall2013Lecture5 =
    type MultisetMap<'a when 'a : comparison> = Map<'a,int>

    let inv mm = Map.forall (fun _ t -> t>0) mm

    let insert a n mm = if Map.containsKey a mm then Map.add a ((Map.find a mm)+n) mm else Map.add a n mm

    let union mm1 mm2 = Map.fold (fun acc k t -> insert k t acc) mm2 mm1

module Fall2015


module Fall2015Q3 =
    
    // Assuming the integer represents the n amount of m3/s
    // let riv1 = R("R1", 5, [])
    // let riv4 = R("R4", 2, [])
    // let riv3 = R("R3", 8, [])
    // let riv2 = R("R2", 15, [R("R4", 2, [])]
    // let riv = R("R", 10, [R("R1", 5, []);R("R2", 15, [R("R4", 2, [])];R("R3", 8, [])]

    // let rec contains (n:Name) (r:River) : bool = 
    // match r with
    // | (name, _, _) when name == n -> true
    // | (_, _, t) -> List.exists (fun x -> contains n x) t
    // | _ -> false

    // let rec allNames (r:River) : string list =
    // match r with
    // | (n,_,[]) -> [n]
    // | (n,_,h::tail) -> [n]::[h]::allNames tail
    // | _ -> []

    // let rec totalFlow (r:River) : int =
    // match r with
    // | (_,f,[]) -> f
    // | (_,f,t) -> (List.reduce (fun x -> totalFlow x) t) + f 
    // | _ -> 0

    // let rec mainSource (r:River) : (Name * Flow) =
    // match r with
    // | (n, f, []) -> (n,f)
    // | (n, f, t) when f > snd (List.max (fun (_, nf) -> nf) (List.map (fun x -> mainSource x) t)) -> (n,f)
    // | _ -> snd (List.max (fun (_, nf) -> nf) (List.map (fun x -> mainSource x) t))

    // let rec tryInsert (n:Name) (t:River) (r:River) : River option =
    // match r with
    // | (na,f,ta) when not (na == n) && not (contains n r) -> Some (na, f, ta::t)
    // | _ -> None

    // Testing syntax:

    type Name = string
    type Flow = int
    type River = R of Name * Flow * River list
    type Tributaries = River list

    let riv1 = R("R1", 5, [])
    let riv4 = R("R4", 2, [])
    let riv3 = R("R3", 8, [])
    let riv2 = R("R2", 15, [R("R4", 2, [])])
    let riv = R("R", 10, [R("R1", 5, []);R("R2", 15, [R("R4", 2, [])]);R("R3", 8, [])])

    let rec contains (n:Name) (r:River) : bool = 
         match r with
         | R (name, _, _) when name = n -> true
         | R (_, _, t) -> List.exists (fun x -> contains n x) t
         | _ -> false

    let getName (r:River) : string = match r with | R (name,_,_) -> name

    let rec allNames (r:River) : string list =
         match r with
         | R (n,_,[]) -> [n]
         | R (n,_,t) -> n::List.collect allNames t
         | _ -> []

    let rec totalFlow (r:River) : int =
         match r with
         | R (_,f,[]) -> f
         | R (_,f,t) -> (List.sumBy (fun x -> (totalFlow x)) t) + f 
         | _ -> 0

    let rec mainSource (r:River) : (Name * Flow) =
         match r with
         | R (n, f, []) -> (n,f)
         | R (n, f, t) when f > snd (List.maxBy (fun (_, nf) -> nf) (List.map (fun x -> mainSource x) t)) -> (n,f)
         | R (_, _, t) -> (List.maxBy (fun (_, nf) -> nf) (List.map (fun x -> mainSource x) t))

    let rec tryInsert (n:Name) (t:River) (r:River) : River option =
         match r with
         | R (na,f,ta) when not (na = n) && not (contains n r) -> Some (R(na, f, t::ta))
         | _ -> None


    // It would be very hard to map the directions with the current model. Assuming the main source flows to other sources, ie. Bigger source to smaller sources. 
    // The model allows for inserting rivers where ever, where creating an invalid river is possible: ie. one river with large flow with 1 tributary with small flow which has one tributary with large flow.
    // Again it would be hard to determine the direction.

    printf "hejsa"





module Fall2015Q2 =
    // 1, 2.1, 3

    // g1 = (a' -> bool) -> a' list
    // g1 computes a list. Essentially computes a list as long as it satisfies a predicate. (Typical TakeWhile behavior.)

    // g2, n= int, h og f er det samme f : a' -> a'
    // g2 = (a' -> a') -> (a'-> a') -> int -> a'

    
    let rec g1r p a = 
        function
        | x::xs when p x -> g1r p xs (x::a)
        | _ -> []
                    
    // g2 is tail recursive as it calculates things first then call it self, avoiding pending calculations. 
    // As an example, to call g2 again, the new n: (n-1), the return of (f x) will be calculated first then be used in the recursive call, doing the work up front.
    // This consequently means the current stack frame is no longer needed. ie. return (return (return (return 5)))) == return 5. There is no longer any pending calculations needed. 

    let rec g1r_cont p a k = 
        function
        | x::xs when p x -> g1r_cont p xs (fun res -> k (res)) a
        | _ -> []



    printf "hejsa"
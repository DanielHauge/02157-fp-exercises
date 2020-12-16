module Fall2016

module Q1 =

    type Name = string
    type Event = string
    type Point = int
    type Score = Name * Event * Point

    type Scoreboard = Score list

    let sb = [("Joe", "June Fishing", 35); ("Peter", "May Fishing", 30);("Joe", "May Fishing", 28); ("Paul", "June Fishing", 28)]

    let thd s = match s with | (_,_,t) -> t | _ -> failwith "was not triple"

    let rec inv (s:Scoreboard) : bool = 
        match s with 
        | [] -> true
        | (_,_,p)::_ when p < 0 -> false
        | (_,_,p)::((_,_,p1)::_) when p < p1 -> false 
        | _::rest -> inv rest
        

    let rec insert (s:Score) (sb:Scoreboard) : Scoreboard =
        let (_,_,p1) = s
        match sb with
        | [] -> [s]
        | (_,_,p)::_ when p < p1 -> s::sb
        | x::rest -> x::insert s rest
       
    let get(n:Name,sb:Scoreboard) : (Event*Point) list =
        let filtered = List.filter (fun (name,_,_) -> name=n) sb
        List.map (fun (_,e,p) -> (e,p)) filtered

    let top (k:int) (sb:Scoreboard) : Scoreboard option =
        if k < 0 then None else try Some (List.take k sb) with | _ -> None
    

module Q2 =
    let rec replace a b xs =
        match xs with
        | x::rest when x=a -> b::replace a b rest
        | x::rest -> x::replace a b rest
        | [] -> []

    (* Part 2
        replace : 'a -> 'a -> 'a list -> 'a list
        a,b and elements in xs needs to be same type, as b will be replacing elements in the list when element is equal to a.
    *)

    (* Part 3
        No, replace is not tail recursive. In order to be tail recursive, the recursive call needs to have calculated its arguments in the next call before calling, as a result not adding pending operations to the callstack.
        Adding accumulating paramenter: acc to the function, calculating a correct accumulating list without pending list concatination operations for each call.
    *)
    let rec replace_tail_recursive a b xs acc =
        match xs with
        | x::rest when x=a -> replace_tail_recursive a b rest (b::acc)
        | x::rest -> replace_tail_recursive a b rest (x::acc)
        | [] -> acc

    (* Example call:
        replace_tail_recursive 2 7 [1; 2; 3; 2; 4] []
    *)

module Q3 =
    (* Part 1
        pos : seq<int>      
            It needs to be sequence of integer as + operator is used.
            It computes an infinite series of incrementing integers starting at 0 offset by +1. ie. [1;2;3;4;5...]

        seq1 : seq<int*int>     
             It can be infered to be integers as pos is used, which is integers.
             It computes an infinite series of tuples starting with (0,0) and then both negative and positive tuples with element in pos in both values ie. [(0,0);(1,1);(-1,-1);(2,2);(-2,-2);....]

        val1 : seq<int*int>
             Type can be infered as it is using seq1.
             Computes the first 5 elements of seq1 series.
    *)


    (* Part 2
        seq2 : seq<int*int>
            Type can be infered because it is using nat which is a seq<int>, which can be infered by Seq.initInfinite id. (id : fun x -> x)
            It computes tuples with second value counting up to the first value in the tuple, incrementing first value when equal.

            val2 takes first 10 elements in seq2:
            val2 = [(0,0);(1,0);(1,1);(2,0);(2,1);(2,2);(3,0);(3,1);(3,2);(3,3)]
    *)
    failwith "no code"

module Q4 =
    failwith "not made"

    (* Part 1
        
        Node(A(false), B([1;2;3]))
        Node(A(true), B([1;2;3]))
        Node(A(true), B([3;2;1]))

    *)

    type Tree<'a,'b> = | A of 'a | B of 'b
                       | Node of Tree<'a,'b> * Tree<'a,'b>


    let v1 = Node(A(false), B([1;2;3]))
    let v2 = Node(A(true), B([1;2;3]))
    let v3 = Node(A(true), B([3;2;1]))


    // Part 2
    let rec ACount (t:Tree<'a,'b>) : int =
        match t with
        | Node(a,b) -> (ACount a) + (ACount b)
        | A(_) -> 1
        | _ -> 0

    // Part 3
    let rec subst a a' b b' (t:Tree<'a,'b>) = 
        match t with
        | Node(x,y) -> Node(subst a a' b b' x, subst a a' b b' y)
        | A(res) when res = a -> A(a')
        | B(res) when res = b -> B(b')
        | otherwise -> otherwise

    (* Part 4
    
        g : Tree<'a,'b> -> Tree<'a,'b>
        It computes a tree which has been turned, mirrored.
        
        f : Tree<'a,'b> -> 'a list * 'b list
        Computes a tuple with all A leaves in a list as first value and all B leaves in a list as second value.

    *)

    let rec f = function
                | A a -> ([a], [])
                | B b -> ([], [b])
                | Node(t1,t2) -> let (xs1, ys1) = (fun x -> f x) t1
                                 let (xs2, ys2) = (fun x -> f x) t2
                                 ((fun (x1,x2) -> x1@x2) (xs1,xs2), (fun (y1,y2) -> y1@y2) (ys1,ys2))

module Q5 =
    type T<'a> = N of 'a * T<'a> list

    // Part 1
    let rec toList (t:T<'a>) =
        match t with
        | N(v, []) -> [v]
        | N(v, list) -> v::List.collect id (List.map (fun x -> toList x) list)

    // Part 2
    let rec map f t = 
        match t with
        | N(v,[]) -> N(f v, [])
        | N(v,list) -> N(f v, List.map (fun x -> map f x) list)

    // Part 3
    type Path = int list

    let rec isPath (is:Path) (t:T<'a>) =
        match (is,t) with
        | ([],_) -> true
        | (x::tail,N(_,st)) -> match List.tryItem x st with 
                              | Some item -> isPath tail item 
                              | None -> false

    let rec get (is:Path) (t:T<'a>) : T<'a> =
        match (is,t) with
        | ([],subtree) -> subtree
        | (x::tail,N(_,st)) -> match List.tryItem x st with 
                              | Some item -> get tail item 
                              | None -> failwith "was not valid path"

    let rec tryFindPathTo a (t:T<'a>) : Path option =
            match t with
            | N(v, []) when v <> a -> None
            | N(v,_) when v = a -> Some []
            | N(_, list) -> let paths = List.map (fun x -> (fst x, tryFindPathTo a (snd x))) (List.indexed list)
                            match paths with
                            | [] -> None
                            | (i,Some p)::_ -> Some (i::p)
            | _ -> None
                            


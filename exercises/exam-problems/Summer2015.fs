module Summer2015


module Summer2015Q1 =
    (*
        
        sq : seq<int>
        k : int -> seq<int*int>

        sq is a inifite series of multiples of 3.
        k is generating an infinite series of tuples of multiples of 3 and the multiple of 3 minus an integer 

    *)
        
    let sq = Seq.initInfinite (fun i -> 3*i)
    let k j = seq { for i in sq do yield (i,i-j) }


    (*
    
        
        xs = [0;3;6;9]
        ys = [(0,-2);(3,1);(6,4);(9,7))
    *)


module Summer2015Q2 =

    // Problem 2
    // 1
    let mixMap f xs ys = List.map f (List.zip xs ys)
    // 2
    let unmixMap f g xs = 
        let (x,y) = List.unzip xs
        (List.map f x, List.map g y)

    // 3: Most genereal types
    // mixmap : (a'* -> b*-> (c',d')) -> a' list -> b' list -> (c',d') list
    // unmixmap : (a' -> c') -> (b' -> d') -> (a'*b') list -> (c' list, d' list)





module Summer2015Q4 =
    // Written on paper:
    let isValidCourseDesc desc = snd desc % 5 = 0

    let isValidCourseBase cb = Map.forall (fun _ desc -> isValidCourseDesc desc) cb

    let disjoint a b = Set.count (Set.intersect a b) = 0

    let sumEcts cs cb = Seq.sum (Set.toSeq (Set.map (fun x-> snd (Map.find x cb)) cs))
    
    let isValidCourseGrp cg cb = 
        let isDisjointed = disjoint (fst cg) (snd cg) 
        let mandatoryEcts = sumEcts (fst cg) cb
        let optionalEcts = sumEcts (snd cg) cb
        isDisjointed && mandatoryEcts <= 45 && optionalEcts <= 45 && mandatoryEcts+optionalEcts >= 45






 
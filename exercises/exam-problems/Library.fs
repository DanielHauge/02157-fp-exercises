namespace exam_problems

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



module Fall2013 =

    
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


module Fall2013Lecture5 =
    type MultisetMap<'a when 'a : comparison> = Map<'a,int>

    let inv mm = Map.forall (fun _ t -> t>0) mm

    let insert a n mm = if Map.containsKey a mm then Map.add a ((Map.find a mm)+n) mm else Map.add a n mm

    let union mm1 mm2 = Map.fold (fun acc k t -> insert k t acc) mm2 mm1



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


namespace propositionallogic

module propositionallogic =
    type Prop<'a> = 
        | A of 'a
        | Dis of Prop<'a> * Prop<'a>
        | Con of Prop<'a> * Prop<'a>
        | Neg of Prop<'a>

    let rec sem (p:Prop<'a>) (asg:Set<'a>) =
        match p with
        | A a -> asg.Contains a
        | Dis(a,b) -> sem a asg || sem b asg
        | Con(a,b) -> sem a asg && sem b asg
        | Neg a -> if (sem a asg) then false else true


    let rec toNnf (p:Prop<'a>) : Prop<'a> = 
        match p with
        | Neg(Neg(a)) -> toNnf a
        | Neg(Con(a,b)) -> Dis(toNnf (Neg(a)),toNnf (Neg(b)))
        | Neg(Dis(a,b)) -> Con(toNnf (Neg(a)),toNnf (Neg(b)))
        | Con(a,b) -> Con(toNnf a, toNnf b)
        | Dis(a,b) -> Dis(toNnf a, toNnf b)
        | A a -> A a
        | Neg(A a) -> Neg(A a)

    let rec onDnf (p:Prop<'a>) : bool =
        match p with
        | Con(_, Dis(_,_)) -> false
        | Con(Dis(_,_), _) -> false
        | Dis(a,b) -> onDnf a && onDnf b
        | Con(a,b) -> onDnf a && onDnf b
        | _ -> true

    let rec toDnf (p:Prop<'a>) : Prop<'a> =
        match p with
        | Con(a, Dis(b,c)) -> (Dis(toDnf (Con(toDnf a, toDnf b)), toDnf (Con(toDnf a, toDnf c))))
        | Con(Dis(a,b), c) -> (Dis(toDnf (Con(toDnf a,toDnf c)), toDnf (Con(toDnf b, toDnf c))))
        | Dis(a,b) -> (Dis(toDnf a, toDnf b))
        | Con(a,b) -> 
            let disjuctiveNnf = Con(toDnf a, toDnf b)
            if (onDnf (disjuctiveNnf)) then disjuctiveNnf else toDnf (disjuctiveNnf)
        | Neg a -> Neg( toDnf a)
        | A a -> A a

    let rec ecToList (p:Prop<'a>) : Prop<'a> list =
        match p with
        | Con(a,b) -> ecToList a @ ecToList b 
        | A a -> [A a]
        | Neg a -> [Neg a]
        | Dis(_,_) -> failwith "that should not have happened. Only elementary conjuctions can be used with this."

    let isEcConsis (p:Prop<'a> list) : bool = List.forall (fun x -> not (List.exists (fun y -> (y = Neg(x) || Neg(y) = x)) p)) p

    let rec toECLists (p:Prop<'a>) : Prop<'a> list list =
        let Nnf = (toDnf p)
        match Nnf with
        | Dis(a,b) -> toECLists a @ toECLists b
        | Con(a,b) -> 
            let aEc = ecToList a
            let bEc = ecToList b
            match (isEcConsis aEc, isEcConsis bEc) with
            | (true,true) -> [aEc;bEc]
            | (true, false) -> [aEc]
            | (false, true) -> [bEc]
            | (false,false) -> []
        | a -> [[a]]

    let rec onNnf (p:Prop<'a>) : bool =
        match p with
        | Neg(Neg(_)) -> false
        | Neg(Con(_,_)) -> false
        | Neg(Dis(_,_)) -> false
        | Con(a,b) -> onNnf a && onNnf b
        | Dis(a,b) -> onNnf a && onNnf b
        | A _ -> true
        | Neg(A _) -> true

    let test =
        let p1 = A "a"
        let n1 = Neg(A "a")
        let n2 = Neg(Con(A "a", A "b"))
        let s1 = Set.ofList(["a";"b"])
        printf "simple sem: %b" (sem p1 s1)


    type Finite = 
        | S of string
        | I of int

    let testNnfSematics p (asg:Set<Finite>) = (sem p asg) = (sem (toNnf p) asg)
    let testNnfProperty p = onNnf (toNnf p)

    let testDnfSemantics p (asg:Set<Finite>) = (sem p asg) = (sem (toDnf p) asg)
    let testDnfProperty p = onDnf (toDnf (toNnf p))

   


    let testProperties =
        FsCheck.Check.Quick testNnfProperty
        FsCheck.Check.Quick testNnfSematics
        FsCheck.Check.Quick testDnfSemantics
        FsCheck.Check.Quick testDnfProperty
        //printf "%b" (isEcConsis (ecToList (Con(Neg(A "a"), A "a"))))



    type Inhabitant = { K1: bool;K2: bool; K3: bool}


        
        
        
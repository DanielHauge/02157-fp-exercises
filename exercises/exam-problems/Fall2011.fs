module Fall2011

module Fall2011Q2 =
    
    // 1  BinOp(BinOp(C 2, "-", C 1), "+", C 5)
    // 2 C 5
    // 3 C(3)

    type exp = | C of int
               | BinOp of exp * string * exp
               | Id of string
               | Def of string * exp * exp

    
    let a1 = BinOp(BinOp(C 2, "-", C 1), "+", C 5)
    let a2 = C 5
    let a3 = C(3)

    let rec toString (e:exp) : string =
        match e with
        | C numb -> numb.ToString()
        | BinOp(e1, op, e2) -> "("+(toString e1)+op+(toString e2)+")"

    let rec getOperators (e:exp) : string list =
        match e with
        | C n -> []
        | BinOp(e1,op,e2) -> op::getOperators e1 @ getOperators e2

    
    let rec getDefs (e:exp) : string list = 
        match e with
        | Id i -> [i]
        | BinOp(e1,_,e2) -> getDefs e1 @ getDefs e2
        | _ -> []

    let rec getIds (e:exp) : string list =
        match e with
        | Def(i,_,e) -> i::getIds e
        | BinOp(e1,_,e2) -> getIds e1 @ getIds e2
        | _ -> []

    let isDef (e:exp) : bool =
        let allDefs = getDefs e
        let allIds = getIds e
        List.forall (fun x -> List.contains x allDefs) allIds 


    let test = Def("x", C 5, BinOp(Id "x", "+", Id "x"))

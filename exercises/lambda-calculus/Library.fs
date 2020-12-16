namespace lambda_calculus
(* Michael R. Hansen, 17-11-2020             *)
module lambda_calculus =
   
    
    
    type Lambda =  | L of string * Lambda
                   | A of Lambda * Lambda
                   | V of string
                   | O of string
                   | I of int 
                   | B of bool
    
    
    // Part 1
    
    // free: Lambda -> Set<string> 
    let rec free t = 
            
            let rec free_acc t bvar =
                match t with
                | L(a,b) -> free_acc b (a::bvar)
                | V(f) when List.exists (fun v -> v = f) bvar -> Set.ofList([f])  
                | A(b,f) -> Set.union (free_acc b bvar) (free_acc f bvar)
                | _ -> Set.ofList([])
            free_acc t []
    
    
    // nextVar: string -> string --- generates a fresh variable                 
    let nextVar = let n   = ref 0
                  let f x = let xnew = x+"#"+string !n
                            n := 1 + !n
                            xnew
                  f     
    
    // subst: Lambda -> string -> Lambda -> Lambda  --- sustitution that takes case of clashes
    let rec subst t x ta = failwith "To be implemented"
    
    
                               
    
    // red: Lambda -> Lambda option  --- reduces the outermost, leftmost redex, is such redex exists.
    // Makes at most one reduction
    let rec red t = match t with 
                    | A(L(x,t1),t2)                   -> failwith "To be implemented"
                    | A(A(O "=", I a), I b)           -> Some(B(a=b))
                    | A(A(O "+", I a), I b)           -> failwith "To be implemented"
                    | A(A(O "-", I a), I b)           -> failwith "To be implemented"
                    | A(A(O "*", I a), I b)           -> failwith "To be implemented"
                    | A(A(A(O "ite", B true),t1),t2)  -> failwith "To be implemented"
                    | A(A(A(O "ite", B false),t1),t2) -> failwith "To be implemented"
                    | L(x,t)                          -> failwith "To be implemented"
                    | A(t1,t2)                        -> failwith "To be implemented"
                    | _                               -> failwith "To be implemented" 
    
    // reduce: Lambda -> Lambda  --- normal-order reduction of a term                  
    let rec reduce t = failwith "To be implemented"
    
    
    // Examples  
    let t1 = L("x",L("y", A(A(O "+", V "x"), V "y")))
    let t2 = A(t1, I 3)
    
    let t3 = A(t2, I 4)
    
    let v3:Lambda = reduce t3
    
    // Turings fixed-point combinator
    let Y = let t = failwith "To be implemented"
            A(t,t)
    
    // Functional for the factorial function 
    let F:Lambda = failwith "To be implemented"
    
    let fact = A(Y,F)
    
    // Examples
    let fac4 = A(fact,I 4)
    
    let vfac4:Lambda = reduce fac4
    
    let fac8 = A(fact,I 8)
    
    let vfac8:Lambda = reduce fac8
    
    // Part 2
    
    
    (* church numerals *)
    
    // convenient shorthands
    let f = V "f"
    let g = V "g"
    let h = V "h"
    let x = V "x"
    let y = V "y" 
    let u = V "u" 
    let v = V "v"    
    let m = V "m"
    let n = V "n"
    let p = V "p"
        
    
    let zero = L("f", L("x", x))
    let one  = L("f", L("x", A(f,x)))
    let two  = L("f", L("x", A(f, A(f, x))))
    
    let succ = L("n", L("f", L("x", A(A(n, f),A(f, x)))))
    let add = L("m", L("n", L("f",L("x",A(A(m, f), A(A(n, f), x))))))
    let pred = L("n", L("f", L("x",A(A(A(n, L("g",L("h", A(h, A(g,f))))),L("u",x)),L("u",u)))))
    
    // toInt: Lambda -> int   --- converts a Church numeral to an integer
    
    // Test of toInt
    
    // Test of succ, add, pred and mult
    
    // mult: Lambda    --- multiplication of Church numerals
    let mult:Lambda = failwith "To be implemented"
    
    // Test of mult
    
    
    // Church Booleans
    
    let True  = L("x", L("y", x))
    let False = L("x", L("y", y))
    let ITE   = L("p",L("x",L("y",A(A(p,x),y))))
    
    // Test of ITE
    
    
    // Test of IsZero
    let IsZero = L("n",A(A(n,L("x",False)),True))
    
    // Test IsZero
    
    
    // Make en new declaration for F -- the functional for the factorial function 
    let F' = failwith "To be implemented"
  
    let factNew = A(Y, F')
    
    // Test of factorials
    
    let vfact4:Lambda = reduce (A(factNew, A(A(add,A(succ, one)),two)))
    
    
    // Try to compute 5! and be a little patient
    
    

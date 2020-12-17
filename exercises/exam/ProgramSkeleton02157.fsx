(* Problem 1 *)

type Person = string
type Contacts = Person list 
type Register = (Person * Contacts) list

let reg1 = [("p1", ["p2"; "p3"]);         
            ("p2", ["p1"; "p4"]); 
            ("p3", ["p1"; "p4"; "p7"]);       
            ("p4", ["p2"; "p3"; "p5"]);
            ("p5", ["p2"; "p4"; "p6"; "p7"]); 
            ("p6", ["p5"; "p7"]); 
            ("p7", ["p3"; "p5"; "p6"])];;

(* Question 1.1   
*)


(* Question 1.2   
*)

 

let rec insert p ps = if List.contains p ps then ps else p::ps;;

let rec combine ps1 ps2 = List.foldBack insert ps1 ps2;;


(* Question 1.3   
*)


(* Question 1.4   
*)
  

(* Question 1.5   
*)


(* Problem 2 *)

let rec h f xs j = match xs with     
                   | []      -> [] 
                   | x::rest -> f j x :: h f rest (j+1);;

let mapi f xs = h f xs 0;;    
mapi (fun j x -> string j + string x) [0..5];; 

(* Question 2.1   
*)

(* Question 2.2   
*)

(* Question 2.3   
*)

(* Question 2.4   
*)


(* Problem 3 *)

type Name = string;; 
type Part = 
    | S of Name                // Simple part       
    | C of Name * Part list;;  // Composite part 
                               // where the list is not empty 

(* Question 3.1 
*)

(* Question 3.2 
*)


(* Question 3.3 
*)


(* Question 3.4 
*)

(* Question 3.5 
*)

type OccurrenceCount =  Map<Name,int>

(* Question 3.6 
*)

 
(* Problem 4 *)

let rec gC i k = 
    if i=0 then k 0
    else if i=1 then k 1
         else gC (i-1) (fun v1 -> gC (i-2) (fun v2 -> k(v1+v2)));;

(* Question 4.1 
*)

(* Question 4.2 
*)

(* Question 4.3 
*)

(* Question 4.4 
*)

     
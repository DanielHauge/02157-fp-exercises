open System

namespace exam_problems

    module exam_problems =

        module P2May2019 =
            type Part = string
            type Task = string
            type Cost = int
            type Duration = int
            type PartReg = Map<Part,Cost>
            type TaskReg = Map<Task,Duration*Cost>

            let preg1 = Map.ofList [("wheel",50); ("saddle",10); ("handlebars",75);("frame",100); ("screw bolt",5); ("nut",3)]
            let treg1 = Map.ofList [("addWheels",(10,2)); ("addSaddle",(5,2));("addHandlebars",(6,1))]

            type WorkStation = Task * (Part*int) list
            type AssemblyLine = WorkStation list

            let ws1 = ("addWheels",[("wheel",2);("frame",1);("screw bolt",2);("nut",2)])
            let ws2 = ("addSaddle",[("saddle",1);("screw bolt",1);("nut",1)])
            let ws3 = ("addHandlebars",[("handlebars",1);("screw bolt",1);("nut",1)])
            let al1 = [ws1; ws2; ws3]

            // Part 1
            let wellDefWS (preg:PartReg) (treg:TaskReg) (ws:WorkStation) : bool = 
                Map.containsKey (fst ws) treg && 
                List.forall (fun x -> Map.containsKey (fst x) preg) (snd ws) && 
                List.forall (fun x-> (snd x) > -1) (snd ws) 

            // Part 2
            let wellDefAL (preg:PartReg) (treg:TaskReg) (al:AssemblyLine) : bool = 
                List.forall (fun x -> wellDefWS preg treg x) al

            // Part 3
            (*
                longest Duration is a function: Assemblyline -> TaskReg -> Duration
            *)
            let longestDuration(al:AssemblyLine,treg:TaskReg) : Duration = 
                List.max (List.map (fun x -> fst (Map.find (fst x) treg)) al)
                

            // Part 4
            let partCostAL (preg:PartReg) (al:AssemblyLine) : Cost =   
                let allParts = List.collect id (List.map (fun x-> (snd x) ) al)
                let partsCosts = List.map (fun x -> (snd x)*(Map.find (fst x) preg)) allParts
                List.sum partsCosts

            // Part 5
            let prodDurCost (treg:TaskReg) (al:AssemblyLine) : Duration*Cost = 
                let durCosts = List.map (fun x-> Map.find (fst x) treg) al
                let total = List.reduce (fun (a,b) (c,d) -> (a+c,b+d)) durCosts
                total
                

            type Stock = Map<Part,int>

            // Part 6
            let toStock (al:AssemblyLine) : Stock = 
                let allParts = List.collect id (List.map (fun x-> (snd x) ) al) // Get all parts for each task.
                let groupedParts = List.groupBy (fun x -> (fst x)) allParts // Group all parts for tasks into parts.
                let collected = List.map (fun x -> (fst x, List.sum (List.map (fun y-> snd y) (snd x)))) groupedParts // Sum each group.
                Map.ofList collected // To hash map.


        module P1May2018 = 

            let rec f xs ys = match (xs,ys) with
            | (x::xs1, y::ys1) -> x::y::f xs1 ys1
            | _ -> []

            (* Part 1
        
                !-> f [1;6;0;8] [0; 7; 3; 3]
                !-> 1::0::f [6;0;8] [7; 3; 3]
                !-> 1::0::6::7::f [0;8] [3; 3]
                !-> 1::0::6::7::0::3::f [8] [3]
                !-> 1::0::6::7::0::3::8::3::f [] []
                !-> 1::0::6::7::0::3::8::3::[]
                -> 1::0::6::7::0::3::8::[3]
                -> 1::0::6::7::0::3::[8;3]
                -> 1::0::6::7::0::[3;8;3]
                -> 1::0::6::7::[0;3;8;3]
                -> 1::0::6::[7;0;3;8;3]
                -> 1::0::[6;7;0;3;8;3]
                -> 1::[0;6;7;0;3;8;3]
                -> [1;0;6;7;0;3;8;3]
        
            *)

            (* Part 2
        
                f : 'a list -> 'a list -> 'a list
                f computes the zipped list. ie. a new list alternating between elements in 2 lists given. (cutting short when one list no longer has any elements)

            *)

            (* Part 3
        
                f is not tail recursive, as each recursive call adds 2 list concatination operations to the callstack

            *) 
            // Part 3 implementation
            let rec f_tr xs ys acc = match (xs,ys) with
                | (x::xs1, y::ys1) -> f_tr xs1 ys1 (x::y::acc)
                | _ -> acc

            // Part 4
            let rec f_tr_cont xs ys k = match (xs,ys) with
            | (x::xs1, y::ys1) -> f_tr_cont xs1 ys1 (fun v -> k x::y::v)
            | _ -> k []


        module P2May2017 =
        

            (* Part 1
        
                f 5 -> Negative argument. as it goes: 5::3::1:: (-1) !!! Fail
                f : int -> int list
                h : seq<'a> -> ('a -> 'b) -> seq<'b>  

                f computes a list of descending even integers from the given integer, failing if given an odd starting integer.
                h formats a sequence given a sequence and a function for how to format. ie. map

            *)


        module P3May2016 =
            type Container = | Tank of float * float * float 
                             | Ball of float 
                             | Cylinder of float * float

            (* Part 1 : Declare 2 values of the Container type.
                let t1 = Tank(5,5,5)
                let b1 = Ball(5)
            *)
            let t1 = Tank(5,5,5)
            let b1 = Ball(5)

            // Part 2
            let isWF (c:Container) : bool = 
                match c with
                | Tank(a,b,c) -> a > 0 && b > 0 && c > 0
                | Ball(r) -> r > 0
                | Cylinder(r,h) -> r > 0 && h > 0

            // Part 3
            let volume (c:Container) : float = 
                match c with
                | Tank(a,b,c) -> a*b*c
                | Ball(r) -> (4/3) * r * r * r * Math.PI
                | Cylinder(r,h) -> h * r * r * Math.PI

            (* Part 4
                Cylinder extension.
            *)

            type Name = string
            type Contents = string
            type Storage = Map<Name, Contents*Container>

            let stor = Storage [ ("tank1", ("oil", Tank(2,2,2))), ("ball1", ("water", Ball(5))) ]


            // Part 5
            let tank1 = failwith "not implemented"
            let oil = failwith "not implemented"
            let ball1 = failwith "ni"
            let water = failwith "ni"

            // Part 6
            let find (n:Name) (s:Storage) : Contents*Storage = 
                Map.find n s

        module P4May16 =
            type T<'a> = L | N of T<'a> * 'a * T<'a>

            let rec f g t1 t2 =
                match (t1,t2) with
                | (L,L) -> L
                | (N(ta1,va,ta2), N(tb1,vb,tb2)) -> N(f g ta1 tb1, g(va,vb), f g ta2 tb2)
        
            let rec h t = match t with
                | L -> L
                | N(t1, v, t2) -> N(h t2, v, h t1)
        
            let rec g = function
                | (_,L) -> None
                | (p, N(t1,a,t2)) when p a -> Some(t1,t2)
                | (p, N(t1,a,t2)) -> match g(p,t1) with
                | None -> g(p,t2)
                | res -> res

            let t = N(N(L, 1, N(N(L, 2, L), 1, L)), 3, L)

            (* Part 1
        
            *)

            (* Part 2
        
            *)

            let count a t = failwith "ni"

            let replace a b t = failwith "ni"
namespace compiler_exercise

module Compiler =
    type Instruction = | ADD | SUB | SIGN | ABS | PUSH of int
    type Exp = | C of int | X | Abs of Exp | Minus of Exp | Sub of Exp * Exp | Add of Exp * Exp
    type Stack = int list

    let intpInstr (s:Stack) (i:Instruction) : Stack = 

        let add (xs:Stack) : Stack =
            match xs with 
            | [] -> []
            | head::tail when not tail.IsEmpty -> (head + tail.Head)::tail.Tail
            | _::tail when tail.IsEmpty -> failwith "tail was empty, only 1 value"
            | _ -> []

        let sub xs =
            match xs with 
            | [] -> []
            | head::tail when not tail.IsEmpty ->  (tail.Head - head)::tail.Tail
            | _::tail when tail.IsEmpty -> failwith "tail was empty, only 1 value"
            | _ -> failwith "could not split"

        let abs xs =
            match xs with
            | [] -> []
            | head::tail when head < 0 -> -head::tail
            | noChange -> noChange 

        let sign xs =
            match xs with
            | [] -> []
            | head::tail -> -head::tail

        let interpret =
            function
            | ADD -> add s
            | SUB -> sub s
            | SIGN -> sign s
            | ABS -> abs s
            | PUSH n -> n::s 

        interpret i

// type Exp = | C of float | X | Abs of Exp | Minus of Exp | Sub of Exp * Exp | Add of Exp * Exp


    let compile (exp:Exp) (i:int) : (Instruction list) = 
        
        let rec ToInstruction =
            function
            | C c -> [PUSH c];
            | Add (a,b) -> ToInstruction a @ ToInstruction b @ [ADD]
            | Sub (a,b) -> ToInstruction a @ ToInstruction b @ [SUB]
            | Abs a -> ToInstruction a @ [ABS]
            | Minus a -> ToInstruction a @ [SIGN]
            | X -> []

        ToInstruction exp


    // minus(abs(2-(5+10))) => -13
    let testExp = Minus(Abs(Sub(C 2, Add(C 5, C 10))))
       

    let exec (ins:Instruction list) : int = (List.fold (fun a -> intpInstr a) [] ins).Head

    let testStack = [PUSH 3; PUSH 7; ADD; PUSH 4; PUSH 5; SUB; ADD]

    let testExecution = 
        printfn "%i" (exec testStack)
        printfn "%i" (exec (compile testExp -13))


    
        
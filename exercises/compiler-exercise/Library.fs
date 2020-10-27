namespace compiler_exercise

module Compiler =
    type Instruction = | ADD | SUB | SIGN | ABS | PUSH of int
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
            | head::tail when not tail.IsEmpty ->  (head - tail.Head)::tail.Tail
            | _::tail when tail.IsEmpty -> failwith "tail was empty, only 1 value"

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
            | _ -> s

        interpret i

    let exec (ins:Instruction list) : int = (List.fold (fun a -> intpInstr a) [] ins).Head
        
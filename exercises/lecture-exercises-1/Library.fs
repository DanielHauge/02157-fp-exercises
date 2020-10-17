namespace lecture_exercises_1



module listops =
    let rec memberOf (x,ys) = 
        match ys with
        | [] -> false
        | y::tail when x=y -> true
        | y::tail -> memberOf (x,tail)

    let rec insert x ys = 
        match ys with
        | [] -> [x]
        | head::tail when x<head -> x::head::tail
        | head::tail -> head::insert x tail

    let sort xs = 
        
        let rec merge a b =
            match (a, b) with
            | ([],_) -> b
            | (_,[]) -> a
            | (aHead::aTail, bHead::btail) when aHead < bHead -> aHead::merge aTail b
            | (_, bHead::bTail) -> bHead::merge a bTail 

        let rec merge_sort x =
            match x with
            | [] -> x
            | [_] -> x
            | [_;_] -> merge ([x.[0]]) ([x.[1]])
            | _ -> merge (merge_sort x.[..(x.Length-2)]) (merge_sort x.[x.Length-1..])

        merge_sort xs
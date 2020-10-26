namespace simple_company_club_exercise

module club =
    type TelephoneNumber = string
    type BirthYear = int
    type Interests = string list
    type Name = string
    type MemberDescription = (TelephoneNumber * BirthYear * Interests)
    type ClubRegister = (Name * MemberDescription) list
    type Arrangement = ((TelephoneNumber -> bool) * (BirthYear -> bool) * (Interests -> bool))
    type TargetGroup = (Name * TelephoneNumber) list

    let rec allExistsWithIn a b = 
        match a with
        | [last] when List.contains last b-> true
        | head::tail when List.contains head b-> allExistsWithIn tail b
        | _ -> false

    let rec oneExistsWithIn a b =
        match a with
        | [] -> false
        | [last] when List.contains last b-> true
        | head::_ when List.contains head b -> true
        | _::tail -> allExistsWithIn tail b

    let reg : ClubRegister = [("Daniel", ("20167840", 1994, ["Gaming"; "Baseball"; "soccer"]));("Maja", ("12345678", 1991, ["ocult"; "antiphofeti"; "jazz"]));("Mor", ("321", 1981, ["ocult"; "soccer"; "jazz"]));("far", ("123", 1985, [ "jazz";"soccer"]))]
    let p1 : Arrangement = ((fun _ -> true), (fun by -> by>1982), (fun intersts -> allExistsWithIn ["jazz";"soccer"] intersts ))
    let p2 : Arrangement = ((fun _ -> true), (fun by -> by>1982), (fun intersts -> oneExistsWithIn ["jazz";"soccer"] intersts ))
    let p3 : Arrangement = ((fun _ -> true), (fun by -> by>1982), (fun intersts -> true))


    let isMemberPartOfTargetArrangement a m = 
        let (telPredicate, birthPredicate, interestsPredicate) = m
        let (telephoneNumber, birth, intersts) = a
        (telPredicate telephoneNumber) && (birthPredicate birth) && (interestsPredicate intersts)



    let rec extractTargetGroup p r = 
       match r : ClubRegister with 
       | [] -> []
       | (name,description)::tail when isMemberPartOfTargetArrangement description p -> 
            let (telephoneNumber, _, _) = description
            (name,telephoneNumber)::extractTargetGroup p tail
       | _ -> []

    let test =
        printfn "Test Arrangement 1: %A" (extractTargetGroup p1 reg)
        printfn "Test Arrangement 2: %A" (extractTargetGroup p2 reg)
        printfn "Test Arrangement 3: %A" (extractTargetGroup p3 reg)
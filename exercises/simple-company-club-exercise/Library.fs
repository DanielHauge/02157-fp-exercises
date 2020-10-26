namespace simple_company_club_exercise

module club =
    type TelephoneNumber = string
    type BirthYear = int
    type Interests = string Set
    type Name = string
    type MemberDescription = (TelephoneNumber * BirthYear * Interests)
    type ClubRegister = Map<Name, MemberDescription>
    type Arrangement = ((TelephoneNumber -> bool) * (BirthYear -> bool) * (Interests -> bool))
    type TargetGroup = (Name * TelephoneNumber) list

    let rec allExistsWithIn a b = 
        match a with
        | [last] when Set.contains last b-> true
        | head::tail when Set.contains head b-> allExistsWithIn tail b
        | _ -> false

    let rec oneExistsWithIn a b =
        match a with
        | [] -> false
        | [last] when Set.contains last b-> true
        | head::_ when Set.contains head b -> true
        | _::tail -> allExistsWithIn tail b

    let reg : ClubRegister = Map.ofList [("Daniel", ("20167840", 1994, Set.ofList ["Gaming"; "Baseball"; "soccer"]));("Maja", ("12345678", 1991, Set.ofList ["ocult"; "antiphofeti"; "jazz"]));("Mor", ("321", 1981, Set.ofList ["ocult"; "soccer"; "jazz"]));("far", ("123", 1985, Set.ofList [ "jazz";"soccer"]))]
    let p1 : Arrangement = ((fun _ -> true), (fun by -> by>1982), (fun intersts -> allExistsWithIn ["jazz";"soccer"] intersts ))
    let p2 : Arrangement = ((fun _ -> true), (fun by -> by>1982), (fun intersts -> oneExistsWithIn ["jazz";"soccer"] intersts ))
    let p3 : Arrangement = ((fun _ -> true), (fun by -> by>1982), (fun intersts -> true))


    let isMemberPartOfTargetArrangement a m = 
        let (telPredicate, birthPredicate, interestsPredicate) = m
        let (telephoneNumber, birth, intersts) = a
        (telPredicate telephoneNumber) && (birthPredicate birth) && (interestsPredicate intersts)


    let extractTargetGroupL5 p r = List.map (fun (name, (telephone,_,_)) -> (name,telephone)) (Map.toList (Map.filter (fun _ description -> isMemberPartOfTargetArrangement description p) r))

    //let rec extractTargetGroup p r = 
    //   match r : ClubRegister with 
    //   | r when Map.isEmpty r -> []
    //   | r when Map.filter (name,description)::tail when isMemberPartOfTargetArrangement description p -> 
    //        let (telephoneNumber, _, _) = description
    //        (name,telephoneNumber)::extractTargetGroup p tail
    //   | _ -> []

    let test =
        printfn "Test Arrangement 1: %A" (extractTargetGroupL5 p1 reg)
        printfn "Test Arrangement 2: %A" (extractTargetGroupL5 p2 reg)
        printfn "Test Arrangement 3: %A" (extractTargetGroupL5 p3 reg)
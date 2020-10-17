namespace utility

module helper =
    let (?) truth trueB falseB =
        if truth then trueB else falseB

    let test_message bool = (?) bool "Pass" "Fail"

    let test result expected = test_message (result = expected)


    let test_l result expected =

        let rec test_recursive res exp =
            match (res, exp) with
            | ([],[]) -> test_message true
            | (_,[]) -> test_message false + " - Result was not empty as expected"
            | ([],_) -> test_message false + " - Actual result was empty" + sprintf " - Expected: %A" expected
            | (x::xtail,y::ytail) when x=y -> test_recursive xtail ytail
            | _ -> (test_message false + " - Actual: " + sprintf "%A" result + sprintf " - Expected: %A" expected)

        test_recursive result expected
            
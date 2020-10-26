namespace misc_exercises

open Microsoft.FSharp.Core


module lecture2blackboard =
    type Location = string
    type Seats = int
    type Computers = int

    type Room = 
    | Auditorium of Location * Seats
    | Databar of Location * Seats * Computers

    let seatCapacity =
        function
        | Auditorium (_,s) -> s
        | Databar (_, s,_) -> s

    let computerCapacity =
        function
        | Auditorium (_,_) -> None
        | Databar (_, _,c) -> Some c 

    let auditorium = Auditorium("hejsa", 2)
    let databar = Databar("hejsa2", 5,5)
    let auditoriumSeats = seatCapacity auditorium
    let auditoriumComputers = computerCapacity auditorium
    let databarSeats = seatCapacity databar
    let databarComputers = computerCapacity databar

    let rec pow x n = 
        match (x,n) with 
        | (_,0) -> 1
        | (x,n) -> x * pow x (n-1)

    let rec squarePlusOne elem = (pow elem 2)+1

    let gg = List.map squarePlusOne [2;3;4;5]

    
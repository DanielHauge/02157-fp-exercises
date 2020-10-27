module week6

// Task 1:
// 1. Der bruges ikke +,-,* eller andre operationer end deconstruction :: og @(append), collect fra list. Derfor er typerne a og b helt generiske, dog er de i lister da functionen bruger list funktioner.
// Hvis for eksempel der blev brugt = eller +, ville typerne kræve plus operator defination eller ligmed defination.
// 2. 
// collect (fun (a,c) -> [a..b]) [(1,3);(4,7);(8,8)]
// (fun (a,c) -> [a..b]) (1,3) @ collect (fun (a,c) -> [a..b]) [(4,7);(8,8)]
// (fun (a,c) -> [a..b]) (1,3) @ (fun (a,c) -> [a..b]) (4,7) @ collect (fun (a,c) -> [a..b]) [(8,8)]
// (fun (a,c) -> [a..b]) (1,3) @ (fun (a,c) -> [a..b]) (4,7) @ fun (a,c) -> [a..b] (8,8)
// [1;2;3] @ (fun (a,c) -> [a..b]) (4,7) @ fun (a,c) -> [a..b] (8,8)
// [1;2;3] @ [4;5;6;7] @ fun (a,c) -> [a..b] (8,8)
// [1;2;3] @ [4;5;6;7] @ [8]
// [1;2;3;4;5;6;7;8]
//
// 3. ((int*int) -> int list) -> (int*int) list -> int list
// Det er den mest generelle type, da integer skal bruges i [a..b] expression for at definere en range. Derfor bliver der resolvet/infered ints

// Task 2:

type Lid = string
type Flight = string
type Airport = string
type Route = (Flight * Airport) list
type LuggageCatalogue = (Lid*Route) List
type ArrivalCatalogue = (Airport*Lid list) list

let exampleCatalogue : LuggageCatalogue = [("DL 016-914", [("DL 189","ATL"); ("DL 124","BRU"); ("SN 733","CPH")]); ("SK 222-142", [("SK 208","ATL"); ("DL 124","BRU"); ("SK 122","JFK")])]
let exampleArrivalCatalogue : ArrivalCatalogue = [("ATL", ["DL 016-914"; "SK 222-142"]); ("BRU", ["DL 016-914"; "SK 222-142"]);("JFK", ["SK 222-142"]);("CPH", ["DL 016-914"])]

let findRoute (info:(Lid*LuggageCatalogue)) : Route = 
    let (lid,cat) = info
    let rec findRoute_rec catalogue =
        match catalogue with
        | [] -> raise (System.ArgumentException("Not found"))
        | (catId,catRoute)::_ when catId=lid -> catRoute
        | _::tail -> findRoute_rec tail
    findRoute_rec cat

let rec inRoute (f:Flight) (r:Route) : bool = 
    match r with
    | [] -> false
    | (fl,_)::_ when fl=f -> true
    | _::tail -> inRoute f tail

let rec withFlight (f:Flight) (lc:LuggageCatalogue) : Lid list =
    match lc with
    | [] -> []
    | (id,route)::tail when inRoute f route -> id::withFlight f tail
    | _::tail -> withFlight f tail

let extend (info:(Lid*Route*ArrivalCatalogue)) : ArrivalCatalogue = 
    let (lid,route,ac) = info
    
    let rec addLidToAirport (a:Airport) (id:Lid) (ac:ArrivalCatalogue) : ArrivalCatalogue =
        match ac with
        | (airport,idList)::tail when airport = a -> (airport,id::idList)::tail
        | head::tail -> head::addLidToAirport a id tail
        | [] -> [(a,[id])]

    let rec addRouteToCatalogue (r:Route) (ac:ArrivalCatalogue) : ArrivalCatalogue =
        match r with
        | (_,ap)::tail -> addLidToAirport ap lid (addRouteToCatalogue tail ac)
        | [] -> ac

    addRouteToCatalogue route ac


let toArrivalCatalogue (lc:LuggageCatalogue) : ArrivalCatalogue =
    List.fold (fun a b -> extend (fst b, snd b, a)) [] lc

    
let inRouteLL (f:Flight) (r:Route) : bool = List.contains f (List.map (fun f-> fst f) r)

let withFlightLL (f:Flight) (lc:LuggageCatalogue) : Lid list = List.map (fun x -> fst x) (List.filter (fun (_,x) -> inRouteLL f x) lc)

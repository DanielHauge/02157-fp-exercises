namespace polynomial_exercise

module polynomial =
    type Poly = int list
    type Degree
    val ofList : int list -> Poly
    val ofArray : int array -> Poly
    val toList : Poly -> int list
    val deg : Poly -> Degree
    val degToString : Degree -> string
    val addD : Degree * Degree -> Degree
    val add : Poly -> Poly -> Poly
    // val ( +. ) : Poly -> Poly -> Poly
    val mul : Poly -> Poly -> Poly
    // val ( *. ) : Poly -> Poly -> Poly
    val sub : Poly -> Poly -> Poly
    // val ( -. ) : Poly -> Poly -> Poly
    val mulX : Poly -> Poly
    val mulC : int -> Poly -> Poly
    val eval : int -> Poly -> int
    val derivative : Poly -> Poly
    val compose : Poly -> Poly -> Poly
    val toString : Poly -> string

// Values can be ⊤, ⊥ either an interval of values [a,b]
// the interval is a sound overapproximation of RIC, such that (a,b,c,d) = (1,ab+d, ac+d, 0) = [ab+d, ac+d]
// ⊤ = [NegativeInf, PositiveInf], ⊥ =  empty set (0,0)

type Values = 
    | Bottom
    | Interval of left : Endpoint * right : Endpoint
    | Top

and Endpoint = 
    | NegativeInf
    | Int of int
    | PositiveInf

;;
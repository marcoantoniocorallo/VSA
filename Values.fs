// Values can be ⊤, ⊥ either an interval of values [a,b]
// the interval is a sound overapproximation of RIC, such that (a,b,c,d) = (1,ab+d, ac+d, 0) = [ab+d, ac+d]
// ⊤ = [NegativeInf, PositiveInf], ⊥ =  empty set (0,0)

type Values = 
    | Bottom
    | Interval of left : Endpoint * right : Endpoint
    | Top

     // ⊤ + ⊥ = ⊥;    ⊤ + ⊤ = ⊤;        ⊤ + I = ⊤;
     //               ⊥ + ⊥ = ⊥;        ⊥ + I = ⊥;
     // I1 : [a,b] + I2 : [c,d] = [a+c,b+d] 
with static member (+) (v1 : Values, v2 : Values) =
         match v1,v2 with
         |Interval(l1,r1),Interval(l2,r2) -> Interval(l1+l2,r1+r2)
         |Bottom,_
         |_,Bottom -> Bottom
         |Top,_ 
         |_,Top -> Top

     // VS1 * VS2 :
     // ⊤ * ⊥ = ⊥;    ⊤ * ⊤ = ⊤;        ⊤ * I = ⊤;
     //               ⊥ * ⊥ = ⊥;        ⊥ * I = ⊥;
     // I1 : [a,b] * I2 : [c,d] = [a*c,b*d] 
     static member (*) (v1 : Values, v2 : Values) =
         match v1,v2 with
         |Interval(l1,r1),Interval(l2,r2) -> Interval(l1*l2,r1*r2)
         |Bottom,_
         |_,Bottom -> Bottom
         |Top,_ 
         |_,Top -> Top

and Endpoint = 
    | NegativeInf
    | Int of int
    | PositiveInf

with static member (+) (e1 : Endpoint, e2 : Endpoint) =
         match e1,e2 with
         |Int(x),Int(y) -> Int(x+y)
         |NegativeInf,PositiveInf 
         |PositiveInf,NegativeInf -> failwith("Undefined sum") 
         |_,PositiveInf 
         |PositiveInf,_ -> PositiveInf
         |_,NegativeInf 
         |NegativeInf,_ -> NegativeInf

     static member (*) (e1 : Endpoint, e2 : Endpoint) = 
         match e1,e2 with
         |Int(x),Int(y) -> Int(x*y)
         |Int(x),PositiveInf when x=0 -> failwith("Undefined product")
         |Int(x),NegativeInf when x=0 -> failwith("Undefined product")
         |PositiveInf,Int(x) when x=0 -> failwith("Undefined product")
         |NegativeInf,Int(x) when x=0 -> failwith("Undefined product")
         |Int(x),PositiveInf when x<0 -> NegativeInf
         |PositiveInf,Int(x) when x<0 -> NegativeInf
         |Int(x),NegativeInf when x<0 -> PositiveInf 
         |NegativeInf,Int(x) when x<0 -> PositiveInf
         |NegativeInf,PositiveInf
         |PositiveInf,NegativeInf -> NegativeInf 
         |PositiveInf,_
         |_,PositiveInf -> PositiveInf
         |NegativeInf,_
         |_,NegativeInf -> NegativeInf 

;;
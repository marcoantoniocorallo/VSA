(* Represents a expression in a CFG node
 *
 * Possible exp types:
 * SimpleAss : R1 = val
 * Ass1 :      R1 = R2 + c
 * Ass2 :      *(R1 + c1) = R2 + c2
 * Ass3 :      R1 = *(R2 + c1 ) + c2
 * LeqConst :  R1 ≤ c
 * GeqConst :  R1 ≥ c
 * LeqVar :    R1 ≤ R2
 *)

type Exp =
    |SimpleAss of string * int
    |Ass1 of string * string * int 
    |Ass2 of string * int * string * int 
    |Ass3 of string * string * int * int
    |LeqConst of string * int 
    |GeqConst of string * int 
    |LeqVar of string * string
;;
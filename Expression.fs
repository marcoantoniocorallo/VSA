(* Represents a expression in a CFG node
 *
 * Ass1 :      R1 = R2 + c
 * Ass2 :      *(R1 + c1) = R2 + c2
 * Ass3 :      R1 = *(R2 + c1 ) + c2
 * LeqConst :  R1 ≤ c
 * GeqConst :  R1 ≥ c
 * LeqVar :    R1 ≤ R2
 *
 * Added by me
 * Global dec : R1, R2,..., Rn
 * SimpleAss : R1 = val		about an aloc global declared
 * Heap dec : R1, R2, ..., Rn
 * SimpleHAss : R1 = val	about an aloc dynamically declared
 *)

type aloc = string;;
type Exp =
    |Ass1 of aloc * aloc * int 
    |Ass2 of aloc * int * aloc * int 
    |Ass3 of aloc * aloc * int * int
    |LeqConst of aloc * int 
    |GeqConst of aloc * int 
    |LeqVar of aloc * aloc

    |GlobalDec of aloc list
    |SimpleAss of aloc * int

    |HeapDec of aloc list
    |SimpleHAss of aloc * int

    |Return 
;;
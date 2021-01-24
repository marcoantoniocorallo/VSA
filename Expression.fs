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
 * Heap dec : R1, R2, ..., Rn
 * SimpleAss : R1 = val
 *)

type ide = string;;
type Exp =
    |Ass1 of ide * ide * int 
    |Ass2 of ide * int * ide * int 
    |Ass3 of ide * ide * int * int
    |LeqConst of ide * int 
    |GeqConst of ide * int 
    |LeqVar of ide * ide

    |SimpleAss of ide * int
    |GlobalDec of ide list
    |HeapDec of ide list
;;
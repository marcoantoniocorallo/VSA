(* Represents a memory region: can be an AR, Heap or Global region 
 * There are two other constructor, not really memory region.
 * Top and Bottom are special value to create the Top/Bot VS element
 *)

type MemoryRegion =
| AR = 0
| Global = 1
| Heap = 2
| Top = 3
| Bottom = 4
;;
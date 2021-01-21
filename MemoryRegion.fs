(* Represents a memory region: can be an AR, Heap or Global region 
 * There are two other constructor, not really memory region.
 * Top and Bottom are special value to create the Top/Bot VS element
 *)

type RegionType =
| AR = 0
| Global = 1
| Heap = 2
| Top = 3
| Bottom = 4
;;

type MemoryRegion(regType : RegionType, id : int) =
    
    member this.ID() = id

    member this.Type() = regType
    
    // this=o <=> this.ID=o.ID and this.Type=o.Type
    override this.Equals(o : obj) =
        let memReg = o:?>MemoryRegion
        if this.ID()=memReg.ID() && this.Type()=memReg.Type() then true else false

    override this.GetHashCode() =
        this.ID().GetHashCode()+this.Type().GetHashCode()
        
    // Implements IComparable in order to use MemoryRegion objs as keys in hashmap
    interface System.IComparable with

        member this.CompareTo(o : obj) = 
            let memReg = o:?>MemoryRegion
            if  this.ID() < memReg.ID() 
                then -1
                else if this.ID() > memReg.ID() 
                    then 1

                    // 0 iff this.Equals(o)=true
                    else if this.Type() = memReg.Type() 
                        then 0
                        else -1

;;
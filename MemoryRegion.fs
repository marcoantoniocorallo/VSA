(* Represents a memory region: can be an AR, Heap or Global region  *)

type RegionType =
| AR = 'A'
| Global = 'G'
| Heap = 'H'
;;

type MemoryRegion(regType : RegionType, id : int) =
    
    member this.ID() = id

    member this.Type() = regType
    
    // this=o <=> this.ID=o.ID and this.Type=o.Type
    override this.Equals(o : obj) =
        let memReg = o:?>MemoryRegion
        if this.ID()=memReg.ID() && this.Type()=memReg.Type() then true else false

    // returns (this.Type().Hash concat this.ID()).Hash
    override this.GetHashCode() =
        let sType = string (this.Type().GetHashCode())
        let sID = string (this.ID())
        (sType+sID).GetHashCode()        
        
    override this.ToString() = (string (this.Type())+", "+string (this.ID()))

    // Implements IComparable in order to use MemoryRegion objs as keys in hashmap
    interface System.IComparable with

        member this.CompareTo(o : obj) : int = 
            let memReg = o:?>MemoryRegion
            if  this.ID() < memReg.ID() 
                then -1
                else if this.ID() > memReg.ID() then 1

                     // 0 iff this.Equals(o)=true
                     else if this.Equals(memReg) 
                         then 0
                         else -1

;;
(* r-tuple of Reduced Interval Congruence of the form (a,b,c,d) = { aZ + d | Z is an el. of [b,c] } 
 * the constructor takes a (MemoryRegion * (int*int*int*int) ) list, 
 * where the first parameter is the type of Memory Region
 * and the second is the tuple (a,b,c,d)
 *
 * There are two ctor for the Top and Bottom element
 * ⊤ = [ R, ..., R ] that are the sets that contains every other set 
 *     (top over everything else, in according with the sorting operator)
 * ⊥ = [ {}, ..., {} ] that are the empty sets, contained in every other set
 * These els are represented by special values (MemoryRegion.Top/Bottom)
 *)

open FSharp.Collections

type ValueSet( list : (MemoryRegion * (int * int * int * int)) list ) = 

    let mutable tuples = list
    let mutable map = new Map<MemoryRegion, Set<int>>([])
        
    // calculates RIC from the tuple
    let rec calcSet (a, b, c, d) = 
        if b=c then [a*b+d]
        else [a*b+d]@(calcSet (a, (b+1), c, d))

    // Fills the map
    let rec f l = 
        match l with
        |[] -> map
        |(memReg,(a,b,c,d))::xs -> map <- map.Add(memReg, new Set<int>(calcSet (a,b,c,d))) ; f xs

    // initializes hashmap 
    do map <- f list

    // HashMap <memReg, RIC>
    member this.VS = map

    // returns the list of keys/memory regions
    member this.MemRegs = this.VS |> Map.toList |> List.map fst

    // Adds or modifies element with a new tuple
    member this.Add(memReg, (a,b,c,d)) = 
        map <- map.Add(memReg, new Set<int>(calcSet(a,b,c,d)));
        tuples <- tuples@[(memReg,(a,b,c,d))]

    // returns the list of tuples 
    member this.Tuples() = tuples

    // Ctor for top and bottom element and methods for work with these special values
    new(s : string) = 
        match s with 
        |"top" -> ValueSet([new MemoryRegion(RegionType.Top, -1),(0,0,0,0)]) 
        |"bot" |"bottom" -> ValueSet([new MemoryRegion(RegionType.Bottom, -1),(0,0,0,0)]) 
        |_ -> ValueSet([])
        
    member this.IsBot() = try 
                            this.VS.[new MemoryRegion(RegionType.Bottom,-1)] |> ignore
                            true
                          with | :? System.Collections.Generic.KeyNotFoundException -> false

    member this.IsTop() = try 
                            this.VS.[new MemoryRegion(RegionType.Top, -1)] |> ignore
                            true
                          with | :? System.Collections.Generic.KeyNotFoundException -> false
    
;;
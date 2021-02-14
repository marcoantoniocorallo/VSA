(* r-tuple of Reduced Interval Congruence of the form (a,b,c,d) = { aZ + d | Z is an el. of [b,c] } 
 * the constructor takes a (MemoryRegion * (int*int*int*int) ) list, 
 * where the first parameter is the type of Memory Region  (AR/Global/Heap)
 * and the second is the tuple (a,b,c,d)
 *
 * There are two ctor for the Top and Bottom element
 * ⊤ = [ R, ..., R ] that are the sets that contains every other set 
 *     (top over everything else, in according with the sorting operator)
 * ⊥ = [ {}, ..., {} ] that are the empty sets, contained in every other set
 *
 * Note: since the set {0} can be represented in several ways, the following RIC are reserved:
 * (0,0,0,0)   -> {0}
 * (0,-1,-1,0) ->  ⊥
 * (0,1,1,0)   ->  ⊤
 *)

open System.Collections.Generic

// list of all MRs and them ID, in order to initialize them value with ⊥
// if the MRs list will become dynamic in the future (AR1, AR2,... ARi,..., ARn; Heap1,..., Heapi,... Heapn),
// then we will simply do a preliminary analysis to become aware of all possible memory regions.
let availableMRs = [new MemoryRegion(RegionType.AR,0);new MemoryRegion(RegionType.Global,1)]

type ValueSet( list : (MemoryRegion * (int * int * int * int)) list ) = 

    // check the correctness of the input parameter
    let check = if list = [] then failwith("Empty input list" ) else ignore()

    // for simpleness there are two maps: one for the tuples (a,b,c,d), the other one for the correspondent set
    let tuples = new Dictionary<MemoryRegion, (int*int*int*int)>()
    let sets = new Dictionary<MemoryRegion, Set<int>>()
        
    // calculates RIC from the tuple
    let rec calcSet (a, b, c, d) = 
        if b=c then [a*b+d]
        else [a*b+d]@(calcSet (a, (b+1), c, d))

    // Fills the maps
    let rec f l = 
        match l with
        |[] -> ignore()
        |(memReg,(a,b,c,d))::xs -> 
            // Add the couple in the maps of sets and tuples
            sets.Add(memReg, new Set<int>(calcSet (a,b,c,d))) ; 
            tuples.Add(memReg, (a,b,c,d)) ;
            f xs

    // Adds no-declared MRs, initializing them with the value ⊥ (i.e. the RIC (0,-1,-1,0))
    let g l = 
        let newl = 
            (availableMRs |> List.filter (fun x -> if l |> List.map fst |> List.contains x then false else true) 
                 |> List.map (fun x -> (x,(0,-1,-1,0))) )@l
        in f newl

    // initializes the hashmaps 
    do
        check
        g list
        
    // Ctor for top and bottom element and methods for work with these special values
    new(mr : MemoryRegion, s : string) = 
        match s with 
        |"top" -> ValueSet([mr,(0,1,1,0)]) 
        |"bot" |"bottom" -> ValueSet([mr,(0,-1,-1,0)]) 
        |_ -> ValueSet([])

    (************************ Methods and attributes : ************************)
    // references the local vars so that they can be modified (this.Add(...))
    member this.Sets = sets
    member this.Tuples = tuples
       
    // returns the list of keys/memory regions
    member this.MemRegs() = this.Sets.Keys

    // Adds new element (memoryRegion, RIC)
    // note: if the memoryRegion already exists, the value will be overwritten
    member this.Add(memReg, (a,b,c,d)) = 
        try
            let key = this.MemRegs() |> Seq.find (fun (x : MemoryRegion) -> if x=memReg then true else false)
            in this.Sets.[key] <- new Set<int>(calcSet(a,b,c,d)) ;  this.Tuples.[key] <- (a,b,c,d) ; ignore()
        with | :? System.Collections.Generic.KeyNotFoundException ->
            this.Sets.Add(memReg, new Set<int>(calcSet(a,b,c,d))) ; this.Tuples.Add(memReg, (a,b,c,d))

    // is a memoryRegion mr ⊤/⊥ ?
    member this.IsBotOf( mr : MemoryRegion ) = 
        try 
            let key = this.MemRegs() |> Seq.find (fun (x : MemoryRegion) -> if x=mr then true else false)
            match this.Tuples.[key] with |(_,-1,-1,_) -> true |(_,_,_,_) -> false
        with | :? System.Collections.Generic.KeyNotFoundException -> false

    member this.IsTopOf( mr : MemoryRegion ) = 
        try 
            let key = this.MemRegs() |> Seq.find (fun (x : MemoryRegion) -> if x=mr then true else false)
            match this.Tuples.[key] with |(_,1,1,_) -> true |(_,_,_,_) -> false
        with | :? System.Collections.Generic.KeyNotFoundException -> false

    // VS is ⊤/⊥ iff memreg = ⊤/⊥ for each memreg in this VS
    member this.IsBot() = this.MemRegs() |> Seq.forall (fun (x : MemoryRegion) -> this.IsBotOf(x) )

    member this.IsTop() = this.MemRegs() |> Seq.forall (fun (x : MemoryRegion) -> this.IsTopOf(x) )
    
    // Like MemRegs/Sets/Tuples, but shows the MemoryRegion in a more readable way
    member this.ShowMemRegs() = this.Sets.Keys |> Seq.toList |> List.map (fun (x : MemoryRegion) -> (x.Type(), x.ID()))

    member this.ShowSets() = this.Sets |> Seq.toList |> List.map (fun ( x : KeyValuePair<MemoryRegion, Set<int>> ) -> ( x.Key.Type(), x.Key.ID() ), x.Value )

    member this.ShowTuples() = this.Tuples |> Seq.toList |> List.map (fun ( x : KeyValuePair<MemoryRegion, (int*int*int*int)> ) -> ( x.Key.Type(), x.Key.ID() ), x.Value )

;;
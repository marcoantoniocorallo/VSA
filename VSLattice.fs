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
let availableMRs = [new MemoryRegion(RegionType.AR,0);new MemoryRegion(RegionType.Global,1);]

type ValueSet( list : (MemoryRegion * (int * int * int * int)) list ) = 
    class

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
            |_ -> ValueSet([] : (MemoryRegion * (int * int * int * int)) list )    // raise exception

        new(s : string) =
            match s with
            |"top" -> ValueSet(availableMRs |> List.map (fun (x : MemoryRegion) -> (x,(0,1,1,0))))
            |"bot" | "bottom" -> ValueSet(availableMRs |> List.map (fun (x : MemoryRegion) -> (x,(0,-1,-1,0))))
            |_ -> ValueSet([] : (MemoryRegion * (int * int * int * int)) list )    // raise exception

        new( l : (MemoryRegion * string) list) =
            ValueSet(l |> List.map (fun (x : (MemoryRegion * string) ) -> 
                (fst x, if snd x = "top" then (0,1,1,0) 
                        else if snd x = "bot" || snd x = "bottom" then (0,-1,-1,0)
                        else failwith("Incorrect input") ) )) 
    
        (**************************************************************************)
        (************************ Methods and attributes **************************)
        (**************************************************************************)
    
        // references the local vars so that they can be modified (this.Add(...))
        member this.Sets = sets
        member this.Tuples = tuples
           
        // returns a list of keys/memory regions
        member this.MemRegs() = this.Sets.Keys |> Seq.toList
    
        // Adds new element (memoryRegion, RIC)
        // note: if the memoryRegion already exists, the value will be overwritten
        member this.Add(memReg, (a,b,c,d)) = 
            try
                let key = this.MemRegs() |> Seq.find (fun (x : MemoryRegion) -> if x=memReg then true else false)
                in this.Sets.[key] <- new Set<int>(calcSet(a,b,c,d)) ;  this.Tuples.[key] <- (a,b,c,d) ; ignore()
            with | :? System.Collections.Generic.KeyNotFoundException ->
                this.Sets.Add(memReg, new Set<int>(calcSet(a,b,c,d))) ; this.Tuples.Add(memReg, (a,b,c,d))
        member this.Add(memReg, s : Set<int>) = 
            try
                let key = this.MemRegs() |> Seq.find (fun (x : MemoryRegion) -> if x=memReg then true else false)
                in this.Sets.[key] <- s ;  this.Tuples.[key] <- (1,s.MinimumElement, s.MaximumElement, 0) ; ignore()
            with | :? System.Collections.Generic.KeyNotFoundException ->
                this.Sets.Add(memReg, s) ; this.Tuples.Add(memReg, (1,s.MinimumElement, s.MaximumElement, 0))    

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
    
    end

    (**************************************************************************)
    (******************************* Lattice **********************************)
    (**************************************************************************)

    interface ILattice<AbstractState> with
    (* "The following operators are defined for value-sets. 
     *  All operators are pointwise applications of the corresponding RIC operator"
     *  
     * vs1 < vs2 SSE vs1[i] ⊆ vs2[i] per i=0,...,vs.Size (! Potrebbero essere inconfrontabili !)
     * vs1 JOIN vs2 -> RICvs1-i JOIN RICvs2-i per i=0,...,vs.Size
     * vs1 MEET vs2 -> RICvs1-i MEET RICvs2-i per i=0,...,vs.Size
     *)

        // Problema: this.Bot()/Top() sono funzioni : unit -> AbstractState; 
        // ma come faccio a restituire un abstractState se non ho indicazioni sulle a-loc in gioco?
        // se le chiedessi come parametro diventerebbero funzioni : a-loc -> AbstractState,
        // il che non matcha con la signature del metodo di ILattice
        // Bottom element = (<aloc-0,⊥>;...;<aloc-n,⊥>)
        member this.Bot() = new AbstractState()
        
        // Top element = (<aloc-0,⊤>;...;<aloc-n,⊤>)
        member this.Top() = new AbstractState()
        
        // Returns true if the value-set states1.[aloc] is a subset of states2.[aloc], for each aloc
        // false otherwise
        member this.Leq states1 states2 = 
            
            // scan list of a-locs
            let a_locs = Seq.toList states1.Keys
            let rec f alocs = 
                match alocs with
                |[] -> true
                |al::als -> 

                    // scan list of memory-region about x's VS 
                    let rec g memregs =
                        match memregs with
                        |[] -> f als
                        |mr::mrs -> 

                            // Compare the VS of the same memory-region for the same a-loc
                            // states1.[al].Sets.[mr] with states2.[al].Sets.[mr] 
                            match (states1.[al].IsBotOf(mr), states1.[al].IsTopOf(mr),
                                   states2.[al].IsBotOf(mr), states2.[al].IsTopOf(mr)) with

                            // ⊥ < something or something < ⊤        
                            |(true,_,_,_)
                            |(_,_,_,true) -> g mrs
                            
                            // ⊤ < something or something < ⊥
                            |(_,true,_,_)
                            |(_,_,true,_) -> false

                            |(_,_,_,_) -> if states1.[al].Sets.[mr].IsSubsetOf(states2.[al].Sets.[mr])
                                          then g mrs else false

                    in g (states1.[al].MemRegs())
            in 
            try f a_locs
            with | :? System.Collections.Generic.KeyNotFoundException -> false
                    
        // Returns the union (join) of states1.[aloc] and states2.[aloc] for each aloc
        // if exist aloc : aloc ∈ states1 ^ aloc ∉ states2 
        //    -> ( states1[aloc] U states2[aloc] = states1[aloc] U ⊥ ) and vice versa
        // Note: Over-approxime the tuple (a,b,c,d) = (vs1 U vs2) with the tuple (1,min,max,0)
        member this.Join states1 states2 = 

            let newState = new AbstractState(states1)

            // union between two VS (sub-join)
            let JOIN (vs1 : ValueSet) (vs2 : ValueSet) : ValueSet = 
                let newVS = new ValueSet(vs1.MemRegs() |> 
                                         List.map (fun (x : MemoryRegion) -> (x,vs1.Tuples.[x]))) 
                let rec g memregs = 
                    match memregs with
                    |[] -> newVS
                    |x::xs -> match (vs1.IsBotOf(x), vs1.IsTopOf(x), vs2.IsBotOf(x), vs2.IsTopOf(x)) with
                              |(true,_,_,_)
                              |(_,_,_,true) -> newVS.Add(x, vs2.Tuples.[x]) ; g xs
                              |(_,true,_,_)
                              |(_,_,true,_) -> g xs

                              |(_,_,_,_) -> newVS.Add(x, new Set<int>(vs1.Sets.[x]+vs2.Sets.[x])) ; g xs
     
                in g (vs2.MemRegs())

            // scan states2's aloc
            let alocs = Seq.toList states2.Keys
            let rec f a_loc =
                match a_loc with
                |[] -> newState
                |x::xs -> 
                    
                    // if current key not exists in states1 -> enter it
                    // otherwise, states1.[key] = (states1.[key] JOIN states2.[key])
                    if states1.ContainsKey x then newState.[x] <- JOIN states1.[x] states2.[x] ; f xs
                    else newState.Add(x,states2.[x]) ; f xs

            in f alocs

        (** Operazioni non essenziali per il momento **)

        // TODO: 
        member this.Meet vs1 vs2 = vs1            

        // TODO:
        member this.Widen vs1 vs2 = vs1

        // TODO:
        member this.Narrow vs1 vs2 = vs1

        // TODO:
        member this.FiniteHeight() = true

        // TODO:
        member this.IsBotEmpty() = true

        // TODO:
        member this.IsBot vs = true

        // TODO:
        member this.IsTop vs = true

    end

and AbstractState = Dictionary<aloc,ValueSet>;;
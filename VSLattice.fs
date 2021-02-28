(* ValueSet is a r-tuple of Reduced Interval Congruence (RIC) 
 * of the form (a,b,c,d) = { aZ + d | Z is an el. of [b,c] } 
 * the constructor takes a (MemoryRegion * RIC ) list.
 *
 * There are two ctor for the Top and Bottom element
 * ⊤ = [ R, ..., R ] that are the sets that contains every other set 
 *     (top over everything else, in according with the sorting operator)
 * ⊥ = [ {}, ..., {} ] that are the empty sets, contained in every other set
 *)

open System.Collections.Generic

type ValueSet( list : (MemoryRegion * RIC) list ) = 
    
    class

        // check the correctness of the input parameter
        let check = if list = [] then failwith("Empty input list" ) else ignore()
    
        // stores the pairs of the input list in a map
        let tuples = new Dictionary<MemoryRegion, RIC>()
            
        // calculates RIC from the tuple
        let rec calcSet (a, b, c, d) = 
            if b=c then [a*b+d]
            else [a*b+d]@(calcSet (a, (b+1), c, d))
    
        // Fills the maps
        let rec f l = 
            match l with
            |[] -> ignore()
            |(memReg,ric)::xs -> 
                // Add the couple in the maps of sets and tuples 
                tuples.Add(memReg, ric) ;
                f xs

        // Adds no-declared MRs, initializing them with the value ⊥
        let g l = 
            let newl = 
                (availableMRs |> List.filter (fun x -> if l |> List.map fst |> List.contains x then false else true) 
                     |> List.map (fun x -> (x,Bottom)) )@l
            in f newl
    
        // initializes the hashmap
        do
            check
            g list
            
        // Ctor for top and bottom element and methods for work with these special values
        new(mr : MemoryRegion, ric : RIC) = new ValueSet([mr,ric])

        new(ric : RIC) = new ValueSet(availableMRs |> List.map (fun (x : MemoryRegion) -> (x,ric)))
    
        (**************************************************************************)
        (************************ Methods and attributes **************************)
        (**************************************************************************)
    
        // references the local vars so that they can be modified (this.Add(...))
        member this.Tuples = tuples

        // Creates a map containing a set for each tuple in Tuples
        member this.Sets = 
            let sets = new Dictionary<MemoryRegion, Set<int>>()
            let rec scan tpls = 
                match tpls with
                |[] -> sets
                |(memreg,ric)::xs -> 
                    (match ric with
                    |Top | Bottom -> sets.Add(memreg,new Set<int>([]))
                    |Ric(a,b,c,d) -> sets.Add(memreg,new Set<int>(calcSet(a,b,c,d)))) ;
                    scan xs
            in scan (this.Tuples |> Seq.toList |> List.map (fun (x : KeyValuePair<MemoryRegion,RIC> )-> (x.Key,x.Value)))
           
        // returns a list of keys/memory regions
        member this.MemRegs() = this.Tuples.Keys |> Seq.toList
    
        // Adds new element (memoryRegion, RIC)
        // note: if the memoryRegion already exists, the value will be overwritten
        member this.Add(memReg, ric) = 
            try
                let key = this.MemRegs() |> Seq.find (fun (x : MemoryRegion) -> if x=memReg then true else false)
                in this.Tuples.[key] <- ric
            with | :? System.Collections.Generic.KeyNotFoundException ->
                this.Tuples.Add(memReg, ric)

        member this.Add(memReg, s : Set<int>) = 
            try
                let key = this.MemRegs() |> Seq.find (fun (x : MemoryRegion) -> if x=memReg then true else false)
                in this.Tuples.[key] <- Ric(1,s.MinimumElement, s.MaximumElement, 0)
            with | :? System.Collections.Generic.KeyNotFoundException ->
                this.Tuples.Add(memReg, Ric(1,s.MinimumElement, s.MaximumElement, 0))    

        // fun : (a,b,c,d) -> (a,b,c,d+cnst) for each RIC (a,b,c,d) in this vs
        member this.AdjustByConst(cnst : int) =  
   
            for mr in this.Tuples.Keys do

                // ⊤ + c = ⊤ ; ⊥ + c = c
                match this.Tuples.[mr] with
                |Ric(a,b,c,d) -> this.Add(mr,Ric(a,b,c,d+cnst))
                |Bottom -> this.Add(mr, Ric(1,cnst,cnst,0))
                |Top -> ignore()

        // is a memoryRegion mr ⊤/⊥ ?
        member this.IsBotOf( mr : MemoryRegion ) =  if this.Tuples.[mr]=Bottom then true else false
    
        member this.IsTopOf( mr : MemoryRegion ) =  if this.Tuples.[mr]=Top then true else false
    
        // this is ⊤/⊥ iff memreg = ⊤/⊥ for each memreg
        member this.IsBot() = this.MemRegs() |> Seq.forall (fun (x : MemoryRegion) -> this.IsBotOf(x) )
    
        member this.IsTop() = this.MemRegs() |> Seq.forall (fun (x : MemoryRegion) -> this.IsTopOf(x) )
        
        // Like MemRegs/Sets/Tuples, but shows the MemoryRegion in a more readable way
        member this.ShowMemRegs() = this.Sets.Keys |> Seq.toList |> List.map (fun (x : MemoryRegion) -> (x.Type(), x.ID()))
    
        member this.ShowSets() = this.Sets |> Seq.toList |> List.map (fun ( x : KeyValuePair<MemoryRegion, Set<int>> ) -> ( x.Key.Type(), x.Key.ID() ), x.Value )
    
        member this.ShowTuples() = this.Tuples |> Seq.toList |> List.map (fun ( x : KeyValuePair<MemoryRegion, RIC> ) -> ( x.Key.Type(), x.Key.ID() ), x.Value )
    
    end

    (**************************************************************************)
    (******************************* Lattice **********************************)
    (**************************************************************************)

    interface ILattice<AbstractState> with
    (* "The following operators are defined for value-sets. 
     *  All operators are pointwise applications of the corresponding RIC operator"
     *  
     * vs1 < vs2 IFF vs1[i] ⊆ vs2[i] for i=0,...,vs1.Size
     * vs1 JOIN vs2 -> RICvs1-i JOIN RICvs2-i per i=0,...,vs.Size
     * vs1 MEET vs2 -> RICvs1-i MEET RICvs2-i per i=0,...,vs.Size
     *)

        // Bottom element = (<aloc-0,⊥>;...;<aloc-n,⊥>)
        member this.Bot() = new AbstractState()
        
        // Top element = (<aloc-0,⊤>;...;<aloc-n,⊤>)
        member this.Top() = 
            let tmp = new AbstractState() in 
            availableVars |> List.map (fun x -> (x,ValueSet(Top))) |> List.iter (fun x -> tmp.Add(x)) ; tmp
        
        // Returns true if the value-set states1.[aloc] is a subset of states2.[aloc], for each aloc in s1
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

        // Returns the intersection (meet) of value-sets vs1 and vs2
        member this.Meet vs1 vs2 = vs1            

        (** Operazioni non essenziali per il momento **)

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

    // states -> states[aloc2+const/aloc1] where aloc+c = (a,b,c,d)->(a,b,c,d+const) for each RIC in aloc
    member this.AdjustByC (states : AbstractState) (aloc1 : aloc) (aloc2 : aloc) cnst = 
        let newStates = new AbstractState()
        let ks = states.Keys |> Seq.toList

        // clone abstract state 
        for k in ks do
            let memregs = states.[k].MemRegs()
            let tuples = states.[k].Tuples.Values |> Seq.toList
            let l = List.zip memregs tuples
            newStates.Add(k,new ValueSet(l))

        // subst newStates[aloc2+c/aloc1]
        let mrs = newStates.[aloc2].MemRegs()
        let tps = newStates.[aloc2].Tuples.Values |> Seq.toList
        let newVS = new ValueSet( List.zip mrs tps )
        newVS.AdjustByConst cnst
        newStates.Remove(aloc1) |> ignore
        newStates.Add(aloc1,newVS)
        newStates

and AbstractState = Dictionary<aloc,ValueSet>;;
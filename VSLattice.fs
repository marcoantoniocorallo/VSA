(* ValueSet is a r-tuple of Values, which can be ⊥, ⊤ or an interval of ints;
 * the constructor takes a (MemoryRegion * Values) list.
 *
 * There are two ctor for the Top and Bottom element
 * ⊤ = [ R, ..., R ] that are the sets that contains every other set 
 *     (top over everything else, in according with the sorting operator)
 * ⊥ = [ {}, ..., {} ] that are the empty sets, contained in every other set
 *)

open System.Collections.Generic

type ValueSet( list : (MemoryRegion * Values) list ) = 
    
    class
    
        // stores the pairs of the input list in a local map, so that it can be modified by some methods
        let map = new Dictionary<MemoryRegion, Values>()

        // Fills the map scanning the list
        let listToMap l = l |> List.map (fun (x : (MemoryRegion * Values) ) -> map.Add(fst x, snd x)) |> ignore

        // Adds no-declared MRs, initializing them with the value ⊥
        let addMR l = 
            let newl = 
                (availableMRs |> List.filter (fun x -> if l |> List.map fst |> List.contains x then false else true) 
                     |> List.map (fun x -> (x,Bottom)) )@l
            in listToMap newl

        // Converts any intervals [-inf, inf] to ⊤ and raise exception for illegal intervals
        let checkInterval (el : Values ) : Values = 
            match el with 
            |Interval(l,r) when l = NegativeInf && r = PositiveInf -> Top
            |Interval(l,r) when l>r -> failwith("Wrong Interval")
            |other -> other
    
        // checks not-empty list, converts any interval [-inf,inf] to ⊤, 
        // completes the list with missing MRs and initializes the hashmap
        let init =
            if list=[] then failwith("Empty input list")
            else list |> List.map (fun ((mr, v) : MemoryRegion * Values) -> (mr, checkInterval v ) ) |> addMR
        
        do  init
            
        // Ctor for top and bottom element and methods for work with these special values
        new(mr : MemoryRegion, v : Values) = new ValueSet([mr,v])
        new(v : Values) = new ValueSet(availableMRs |> List.map (fun (x : MemoryRegion) -> (x,v)))
    
        (**************************************************************************)
        (************************ Methods and attributes **************************)
        (**************************************************************************)
    
        // references the local map so that it can be modified (this.Add(...))
        member this.Map = map
           
        // returns a list of keys/memory regions
        member this.MemRegs() = this.Map.Keys |> Seq.toList

        // returns a deep copy of this
        member this.Clone() = new ValueSet(this.Map.Values 
                                |> Seq.toList 
                                |> List.zip (this.Map.Keys |> Seq.toList))
    
        // Adds new element (memoryRegion, Values)
        // the existing memoryRegion's value will be overwritten
        member this.AddChange(memReg, values) = 
            let key = this.MemRegs() |> Seq.find (fun (x : MemoryRegion) -> if x=memReg then true else false)
            in  this.Map.[key] <- checkInterval values

        // Adjust each interval in this VS by a constant
        member this.AdjustByConst(cnst : int) =  
            for mr in this.Map.Keys do

                match this.Map.[mr] with
                |Interval(Int(l),Int(r)) -> this.AddChange(mr,Interval(Int(l+cnst),Int(r+cnst)))
                |Interval(NegativeInf, Int(r)) -> this.AddChange(mr,Interval(NegativeInf,Int(r+cnst)))
                |Interval(Int(l), PositiveInf) -> this.AddChange(mr,Interval(Int(l+cnst),PositiveInf))

                // ⊤ + c = ⊤ ; ⊥ + c = c
                |Bottom -> this.AddChange(mr, Interval(Int(cnst),Int(cnst)))
                |Top -> ignore()

                |_ -> failwith("Wrong interval")

        // is a memoryRegion mr ⊤/⊥ ?
        member this.IsBotOf( mr : MemoryRegion ) =  if this.Map.[mr]=Bottom then true else false
        member this.IsTopOf( mr : MemoryRegion ) =  if this.Map.[mr]=Top then true else false
    
        // this is ⊤/⊥ iff memreg = ⊤/⊥ for each memreg
        member this.IsBot() = this.MemRegs() |> Seq.forall (fun (x : MemoryRegion) -> this.IsBotOf(x) )
        member this.IsTop() = this.MemRegs() |> Seq.forall (fun (x : MemoryRegion) -> this.IsTopOf(x) )
    
    end

    (**************************************************************************)
    (******************************* Lattice **********************************)
    (**************************************************************************)

    interface ILattice<AbstractState> with
    (* "The following operators are defined for value-sets. 
     *  All operators are pointwise applications of the corresponding intervals operator"
     *)

        // Bottom element = (<aloc-0,⊥>;...;<aloc-n,⊥>)
        member this.Bot() = new AbstractState()
        
        // Top element = (<aloc-0,⊤>;...;<aloc-n,⊤>)
        member this.Top() = 
            let tmp = new AbstractState() in 
            availableVars |> List.map (fun x -> (x,ValueSet(Top))) |> List.iter (fun x -> tmp.Add(x)) ; tmp
        
        // Returns true if the value-set states1.[aloc] is a subset of states2.[aloc], for each aloc in s1
        // false otherwise
        // vs1 < vs2 IFF vs1[mr] ⊆ vs2[mr] for each memreg mr
        // [a, b] < [c, d] IFF a ≥ c ∧ b ≤ d
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

                            // Compare the values of the same memory-region for the same a-loc
                            // states1.[al].Map.[mr] with states2.[al].Map.[mr] 
                            match (states1.[al].Map.[mr], states2.[al].Map.[mr]) with
                            |(Interval(l1,r1), Interval(l2,r2)) -> if l1>=l2 && r1<=r2 then g mrs else false
                            |values1, values2 -> if values1<=values2 then g mrs else false 

                    in g (states1.[al].MemRegs())
            in 
            try f a_locs
            with | :? System.Collections.Generic.KeyNotFoundException -> false
                    
        // Returns the union (join) of states1.[aloc] and states2.[aloc] for each aloc
        // if exist aloc : aloc ∈ states1 ∧ aloc ∉ states2 
        //    -> ( states1[aloc] U states2[aloc] = states1[aloc] U ⊥ = states1[aloc] ) and vice versa
        // [a, b] U [c, d] = [min(a,c), max(b,d)]
        member this.Join states1 states2 = 

            let newState = new AbstractState(states1)

            // union between two VS (sub-join)
            let JOIN (vs1 : ValueSet) (vs2 : ValueSet) : ValueSet = 
                let newVS = new ValueSet(vs1.MemRegs() |> 
                                         List.map (fun (x : MemoryRegion) -> (x,vs1.Map.[x]))) 
                let rec g memregs = 
                    match memregs with
                    |[] -> newVS
                    |x::xs -> 
                        match (vs1.IsBotOf(x), vs1.IsTopOf(x), vs2.IsBotOf(x), vs2.IsTopOf(x)) with
                        // { ⊤ U _ | _ U ⊥ } -> vs1.[x] 
                        |(_,true,_,_)
                        |(_,_,true,_) -> g xs                              
                        // { ⊥ U _ | _ U ⊤ } -> vs2.[x]
                        |(true,_,_,_)
                        |(_,_,_,true) -> newVS.AddChange(x, vs2.Map.[x]) ; g xs

                        |(_,_,_,_) -> 
                            match vs1.Map.[x], vs2.Map.[x] with 
                            |Interval(a,b),Interval(c,d) -> newVS.AddChange(x,Interval(min a c,max b d))
                            |_,_ -> failwith("Unexpected case")
                            g xs
     
                in g (vs2.MemRegs())

            // scan states2's aloc
            let alocs = Seq.toList states2.Keys
            let rec f a_loc =
                match a_loc with
                |[] -> newState
                |x::xs -> 
                    
                    // if current key not exists in states1 -> enter it
                    // otherwise, states1.[key] = (states1.[key] JOIN states2.[key])
                    if states1.ContainsKey x then newState.[x] <- JOIN states1.[x] states2.[x]
                    else newState.Add(x,states2.[x])
                    f xs

            in f alocs

        // Returns the intersection (meet) of states1.[aloc] and states2.[aloc] for each aloc
        //                        [max(a, c), min(b, d)] if max(a,c) ≤ min(b,d)
        // [a, b] MEET [c, d] = {
        //                        ⊥ otherwise
        member this.Meet states1 states2 = 

            let newAbs = new AbstractState()
            
            // meet between two VS (sub-meet)
            let MEET (vs1 : ValueSet) (vs2 : ValueSet) : ValueSet = 
                let newVS = new ValueSet(Bottom)

                let rec g memregs = 
                    match memregs with
                    |[] -> newVS
                    |x::xs -> 
                        match (vs1.IsBotOf(x), vs1.IsTopOf(x), vs2.IsBotOf(x), vs2.IsTopOf(x)) with
                        // { ⊥ MEET _ | _ MEET ⊥ } -> ⊥
                        |(true,_,_,_)
                        |(_,_,true,_) -> g xs
                        // { ⊤ MEET x | x MEET ⊤ } -> x 
                        |(_,true,_,_) -> newVS.AddChange(x, vs2.Map.[x]) ; g xs
                        |(_,_,_,true) -> newVS.AddChange(x, vs1.Map.[x]) ; g xs

                        |(_,_,_,_) ->
                            match vs1.Map.[x], vs2.Map.[x] with 
                            |Interval(a,b),Interval(c,d) -> 
                                let l = max a c
                                let r = min b d 
                                newVS.AddChange(x, (if l <= r then Interval(l,r) else Bottom ) )
                            |_,_ -> failwith("Unexpected case")                              
                            g xs 

                in g (vs1.MemRegs())

            // Intersection of alocs
            let alocs = ((Set.ofSeq (states1.Keys)) |> Set.intersect (Set.ofSeq (states2.Keys))) |> Set.toList

            let rec f a_locs = 
                match a_locs with
                |[] -> newAbs
                |x::xs -> newAbs.Add(x, MEET states1.[x] states2.[x]) ; f xs
            in f alocs

        // Returns the abstract state obtained by widening states1.[aloc] 
        // with respect to states2.[aloc] for each aloc
        member this.Widen states1 states2 = states1

        (** Operazioni non essenziali per il momento **)

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

    // states -> states[VSk+c/VSk] where VSk is k's VS = [a,b], and VSk+c = [a+c,b+c]  
    member this.AdjustByC (states : AbstractState) (aloc : aloc) cnst = 
        let newStates = new AbstractState()
        let ks = states.Keys |> Seq.toList

        // clone abstract state 
        for k in ks do
            newStates.Add(k,states.[k].Clone())

        // subst newStates[aloc+c/aloc]
        newStates.[aloc].AdjustByConst(cnst)
        newStates

and AbstractState = Dictionary<aloc,ValueSet>;;
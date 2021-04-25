open System.Collections.Generic

let WideningThreshold = 10;;

// An abstract state is a map: aloc -> VS
// Implementation is more or less a wrapper for Dictionary<aloc,ValueSet> objs, 
// except that it exposes the map instead of providing setter/getter methods
type AbstractState() = 

    let map = new Dictionary<aloc,ValueSet>()

    // n. of widening applications
    let mutable NWidened = 0
    member this.GetNWidened() = NWidened
    member this.IncNWidened() = NWidened <- NWidened+1
    member this.SetNWidened(n) = NWidened <- n

    member this.Map = map

    member this.Keys = this.Map.Keys

    member this.Values = this.Map.Values

    member this.Add(a,b) = this.Map.Add(a,b)

    member this.ContainsKey(key) = this.Map.ContainsKey(key)

    member this.Clone() = 
        let abs = new AbstractState()
        abs.SetNWidened(this.GetNWidened())
        for key in this.Keys do
            abs.Add(key, this.Map.[key].Clone())
        abs

    override this.Equals(o) = 
        let states = o:?>AbstractState
        let alocs1 = this.Keys |> Seq.toList |> List.sort
        let alocs2 = states.Keys |> Seq.toList |> List.sort
        if alocs1<>alocs2 then false else
            let rec f alocs =
                match alocs with
                |[] -> true
                |x::xs -> if this.Map.[x]=states.Map.[x] then f xs else false
            in f alocs1

    override this.GetHashCode() = this.Map.GetHashCode()

    override this.ToString() =
        let rec f keys s = 
            match keys with
            |[] -> s
            |x::xs -> f xs (s+x+": { | "+this.Map.[x].ToString()+"}\n")
        in f (this.Keys |> Seq.toList) ""

(* ValueSet is a r-tuple of Values, which can be ⊥, ⊤ or an interval of ints;
 * the constructor takes a (MemoryRegion * Values) list. 
 * ⊤ = [ R, ..., R ] that are the sets that contains every other set 
 *     (top over everything else, in according with the sorting operator)
 * ⊥ = [ {}, ..., {} ] that are the empty sets, contained in every other set *)
and ValueSet( list : (MemoryRegion * Values) list ) = 
    
    class
    
        // stores the pairs of the input list in a local map, so that it can be modified by some methods
        let map = new Dictionary<MemoryRegion, Values>()

        // Fills the map scanning the list
        let listToMap l = l |> List.map (fun (x : MemoryRegion * Values ) -> map.Add(fst x, snd x)) |> ignore

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
            |Interval(l,r) when l>r -> Interval(r,l)
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
            //let key = this.MemRegs() |> Seq.find (fun (x : MemoryRegion) -> if x=memReg then true else false)
            this.Map.[memReg] <- checkInterval values

        // Adjust/Times each interval in this VS by a constant
        member this.AdjustByConst(cnst : int) =  
            for mr in this.Map.Keys do

                match this.Map.[mr] with
                |Interval(Int(l),Int(r)) -> this.AddChange(mr,Interval(Int(l+cnst),Int(r+cnst) ) )
                |Interval(NegativeInf, Int(r)) -> this.AddChange(mr,Interval(NegativeInf,Int(r+cnst)))
                |Interval(Int(l), PositiveInf) -> this.AddChange(mr,Interval(Int(l+cnst),PositiveInf))

                // ⊤ + c = ⊤ ; ⊥ + c = c
                |Bottom -> this.AddChange(mr, Interval(Int(cnst),Int(cnst)))
                |Top -> ignore()

                |_ -> failwith("Wrong interval")

        member this.TimesByConst(cnst : int) =  
            for mr in this.Map.Keys do

                match this.Map.[mr] with
                |Interval(Int(l),Int(r)) -> this.AddChange(mr,Interval(Int(l*cnst),Int(r*cnst)))
                |Interval(NegativeInf, Int(r)) -> this.AddChange(mr,Interval(NegativeInf,Int(r*cnst)))
                |Interval(Int(l), PositiveInf) -> this.AddChange(mr,Interval(Int(l*cnst),PositiveInf))

                // ⊤ * c = ⊤ ; ⊥ * c = c
                |Bottom -> this.AddChange(mr, Interval(Int(cnst),Int(cnst)))
                |Top -> ignore()

                |_ -> failwith("Wrong interval")

        // Remove Upper/Lower Bounds of each component RIC
        member this.RmUpperBounds() =
            for mr in this.Map.Keys do
                match this.Map.[mr] with
                |Interval(l,r) -> this.AddChange(mr,Interval(l,PositiveInf))
                |_ -> ignore()
        member this.RmLowerBounds() =
            for mr in this.Map.Keys do
                match this.Map.[mr] with
                |Interval(l,r) -> this.AddChange(mr,Interval(NegativeInf,r))
                |_ -> ignore()

        // is a memoryRegion mr ⊤/⊥ ?
        member this.IsBotOf( mr : MemoryRegion ) =  if this.Map.[mr]=Bottom then true else false
        member this.IsTopOf( mr : MemoryRegion ) =  if this.Map.[mr]=Top then true else false
    
        // this is ⊤/⊥ iff memreg = ⊤/⊥ for each memreg
        member this.IsBot() = this.MemRegs() |> Seq.forall (fun (x : MemoryRegion) -> this.IsBotOf(x) )
        member this.IsTop() = this.MemRegs() |> Seq.forall (fun (x : MemoryRegion) -> this.IsTopOf(x) )
    
        // override Equals, ToString, GetHashCode
        override this.Equals(o) = 
            let vs = (o:?>ValueSet)
            let rec f memregs = 
                match memregs with
                |[] -> true
                |x::xs -> if this.Map.[x]=vs.Map.[x] then f xs else false
            in f (this.MemRegs())

        override this.GetHashCode() = this.Map.GetHashCode()

        override this.ToString() =
            let rec f memregs s = 
                match memregs with
                |[] -> s
                |x::xs -> f xs (s+"["+x.ToString()+"] -> ("+this.Map.[x].ToString()+") | ")
            in f (this.MemRegs()) ""

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
            availableVars |> List.map (fun x -> (x,ValueSet(Top))) |> List.iter tmp.Add ; tmp
        
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
                            match (states1.Map.[al].Map.[mr], states2.Map.[al].Map.[mr]) with
                            |Interval(l1,r1), Interval(l2,r2) -> if l1>=l2 && r1<=r2 then g mrs else false
                            |values1, values2 -> if values1<=values2 then g mrs else false 

                    in g (states1.Map.[al].MemRegs())
            in 
            try f a_locs
            with | :? System.Collections.Generic.KeyNotFoundException -> false
                    
        // Returns the union (join) of states1.[aloc] and states2.[aloc] for each aloc
        // if exist aloc : aloc ∈ states1 ∧ aloc ∉ states2 
        //    -> ( states1[aloc] U states2[aloc] = states1[aloc] U ⊥ = states1[aloc] ) and vice versa
        // [a, b] U [c, d] = [min(a,c), max(b,d)]
        member this.Join states1 states2 = 

            let newState = states1.Clone()

            // union between two VS (sub-join)
            let JOIN (vs1 : ValueSet) (vs2 : ValueSet) : ValueSet = 
                let newVS = vs1.Clone()
                let rec g memregs = 
                    match memregs with
                    |[] -> newVS
                    |x::xs -> 
                        match (vs1.IsBotOf(x), vs1.IsTopOf(x), vs2.IsBotOf(x), vs2.IsTopOf(x)) with
                        // { ⊤ U _ | _ U ⊥ } -> vs1.[x] 
                        |_,true,_,_
                        |_,_,true,_ -> g xs                              
                        // { ⊥ U _ | _ U ⊤ } -> vs2.[x]
                        |true,_,_,_
                        |_,_,_,true -> newVS.AddChange(x, vs2.Map.[x]) ; g xs

                        |_,_,_,_ -> 
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
                    if states1.ContainsKey x then newState.Map.[x] <- JOIN (states1.Map.[x].Clone()) (states2.Map.[x].Clone())
                    else newState.Add(x,states2.Map.[x].Clone())
                    f xs

            in f alocs

        // Returns the intersection (meet) of states1.[aloc] and states2.[aloc] for each aloc
        //                        [max(a, c), min(b, d)] if max(a,c) ≤ min(b,d)
        // [a, b] MEET [c, d] = {
        //                        ⊥ otherwise
        member this.Meet states1 states2 = 

            let newAbs = new AbstractState()
            newAbs.SetNWidened(states1.GetNWidened())

            // meet between two VS (sub-meet)
            let MEET (vs1 : ValueSet) (vs2 : ValueSet) : ValueSet = 
                let newVS = new ValueSet(Bottom)

                let rec g memregs = 
                    match memregs with
                    |[] -> newVS
                    |x::xs -> 
                        match (vs1.IsBotOf(x), vs1.IsTopOf(x), vs2.IsBotOf(x), vs2.IsTopOf(x)) with
                        // { ⊥ MEET _ | _ MEET ⊥ } -> ⊥
                        |true,_,_,_
                        |_,_,true,_ -> g xs
                        // { ⊤ MEET x | x MEET ⊤ } -> x 
                        |_,true,_,_ -> newVS.AddChange(x, vs2.Map.[x]) ; g xs
                        |_,_,_,true -> newVS.AddChange(x, vs1.Map.[x]) ; g xs

                        |_,_,_,_ ->
                            match vs1.Map.[x], vs2.Map.[x] with 
                            |Interval(a,b),Interval(c,d) -> 
                                let l = max a c
                                let r = min b d 
                                newVS.AddChange(x, (if l <= r then Interval(l,r) else Bottom ) )
                            |_,_ -> failwith("Unexpected case")                              
                            g xs 

                in g (vs1.MemRegs())

            // Intersection of alocs
            let alocs = ((Set.ofSeq states1.Keys) |> Set.intersect (Set.ofSeq states2.Keys)) |> Set.toList

            let rec f a_locs = 
                match a_locs with
                |[] -> newAbs
                |x::xs -> newAbs.Add(x, MEET states1.Map.[x] states2.Map.[x]) ; f xs
            in f alocs

        // Returns the abstract state obtained by widening states1.[aloc] 
        // with respect to states2.[aloc] for each aloc
        member this.Widen states1 states2 = 

            // for a threshold K of iterations, Widening = Join
            states1.IncNWidened()
            if states1.GetNWidened() <= WideningThreshold 
            then (this:>ILattice<AbstractState>).Join states1 states2
            else
    
                let newStates = states1.Clone()
                    
                let WIDEN (vs1 : ValueSet) (vs2 : ValueSet) k =
                    for memreg in (vs1.MemRegs()) do
                        match vs1.Map.[memreg],vs2.Map.[memreg] with
                        |Interval(l1,r1),Interval(l2,r2) -> 
                            match (l2<l1),(r2>r1) with
                            |true, true -> newStates.Map.[k].AddChange(memreg,Top)
                            |true,_ -> newStates.Map.[k].AddChange(memreg,Interval(NegativeInf,r1))
                            |_,true -> newStates.Map.[k].AddChange(memreg,Interval(l1,PositiveInf))
                            |_,_ -> ignore()
                        // Bottom,_ / _,Top
                        |x,y when y>x -> newStates.Map.[k].AddChange(memreg,y)
                        |_,_ -> ignore()
                    
                // scan states2's aloc
                let alocs = Seq.toList states2.Keys
                let rec f a_loc =
                    match a_loc with
                    |[] -> newStates
                    |x::xs -> 
                        
                        // if current key not exists in states1 -> enter it
                        // otherwise, states1.[key] = (states1.[key] JOIN states2.[key])
                        if states1.ContainsKey x then WIDEN (states1.Map.[x].Clone()) (states2.Map.[x].Clone()) x
                        else newStates.Add(x,states2.Map.[x].Clone()) 
                        f xs
    
                in f alocs

    end

    // states -> states[VSk+c/VSk] where VSk is k's VS = [a,b], and VSk+c = [a+c,b+c]  
    member this.AdjustByC (states : AbstractState) (aloc : aloc) cnst = 
        let newStates = states.Clone()
        newStates.Map.[aloc].AdjustByConst(cnst)
        newStates

    // states -> states[VSk*c/VSk] where VSk is k's VS = [a,b], and VSk*c = [a*c,b*c]  
    member this.TimesByC (states : AbstractState) (aloc : aloc) cnst = 
        let newStates = states.Clone()
        newStates.Map.[aloc].TimesByConst(cnst)
        newStates

    // VS1 + VS2 :
    // ⊤ + ⊥ = ⊤;    ⊤ + ⊤ = ⊤;        ⊤ + I = ⊤;
    //               ⊥ + ⊥ = ⊥;        ⊥ + I = I;
    // I1 : [a,b] + I2 : [c,d] = [a+c,b+d] 
    static member (+) (vs1 : ValueSet, vs2 : ValueSet) =
        let newVS = vs1.Clone()
        for mr in vs1.MemRegs() do

            match vs1.Map.[mr], vs2.Map.[mr] with
            |Interval(Int(l1),Int(r1)),Interval(Int(l2),Int(r2)) -> newVS.AddChange(mr,Interval(Int(l1+l2),Int(r1+r2)))
            |_,Top -> newVS.AddChange(mr,Top)
            |Bottom,Interval(l,r) -> newVS.AddChange(mr,Interval(l,r))
            
            |_ -> ignore()
        newVS

    // VS1 * VS2 :
    // ⊤ * ⊥ = ⊤;    ⊤ * ⊤ = ⊤;        ⊤ * I = ⊤;
    //               ⊥ * ⊥ = ⊥;        ⊥ * I = I;
    // I1 : [a,b] * I2 : [c,d] = [a*c,b*d] 
    static member (*) (vs1 : ValueSet, vs2 : ValueSet) =
        let newVS = vs1.Clone()
        for mr in vs1.MemRegs() do

            match vs1.Map.[mr], vs2.Map.[mr] with
            |Interval(Int(l1),Int(r1)),Interval(Int(l2),Int(r2)) -> newVS.AddChange(mr,Interval(Int(l1*l2),Int(r1*r2)))
            |_,Top -> newVS.AddChange(mr,Top)
            |Bottom,Interval(l,r) -> newVS.AddChange(mr,Interval(l,r))
            
            |_ -> ignore()
        newVS

;;
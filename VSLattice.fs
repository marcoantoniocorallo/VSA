// "The following operators are defined for value-sets. 
//  All operators are pointwise applications of the corresponding RIC operator"
// Comportamento previsto: 
// vs1 < vs2 SSE vs1[i] ⊆ vs2[i] per i=0,...,vs.Size (! Potrebbero essere inconfrontabili !)
// vs1 JOIN vs2 -> RICvs1-i JOIN RICvs2-i per i=0,...,vs.Size
// vs1 MEET vs2 -> RICvs1-i MEET RICvs2-i per i=0,...,vs.Size
// Nel caso in cui uno dei due oggetti non contenga un elemento (Heapi, ARi), viene considerato l'elemento Bottom

open FSharp.Collections
type Lattice() = 

    interface ILattice<AbstractState> with

        // Bottom element = (<aloc-0,⊥>;...;<aloc-n,⊥>)
        member this.Bot() = new AbstractState([("0",new ValueSet("bot"))])
        
        // Top element = (<aloc-0,⊤>;...;<aloc-n,⊤>)
        member this.Top() = new AbstractState([("0",new ValueSet("top"))])
        
        // Returns true if the value-set states1.[aloc] is a subset of states2.[aloc], for each aloc
        // false otherwise
        member this.Leq states1 states2 =

            // Comparable iff the two states have the same Alocs 
            if states1.ALocs <> states2.ALocs then false
            else
                
                // if states1 = ⊥ or states2 = ⊤ -> True
                if states1.IsBot() || states2.IsTop() then true 
                else
                    let mutable res = true 
                    for aloc in states1.ALocs do

                        // Value Set for current aloc
                        let vs1 = states1.Map.[aloc]
                        let vs2 = states2.Map.[aloc]

                        // vs1's list of keys
                        let keys1 = vs1.MemRegs()
                        let rec f (keys : MemoryRegion list) = 
                            match keys with
                            |[] -> res <- res
                            |k::ks -> try 

                                         // ID = -1 -> ⊥; ID = -2 -> ⊤
                                         match k.ID() with 
                                         | -1 -> f ks 
                                         | -2 -> if vs2.VS.[k]=Set<int>([0]) then f ks else res <- false
                                         | _ ->  if not(vs1.VS.[k].IsSubsetOf(vs2.VS.[k])) 
                                                    then res <- false
                                                    else f ks

                                      // vs2 doesn't contain one vs1's Memory-region -> False  
                                      with | :? System.Collections.Generic.KeyNotFoundException -> res <- false
                        in f keys1
                    res
                    
        // Returns the union (join) of states1.[aloc] and states2.[aloc] for each aloc
        // if exist i : i ∈ states1 ^ i ∉ states2 -> ( states1[i] U states2[i] = states1[i] U ⊥ ) and vice versa
        // if exist i : i ∈ vs1 ^ i ∉ vs2 -> ( vs1[i] U vs2[i] = vs1[i] U ⊥ ) and vice versa
        // Note: Over-approxime the tuple (a,b,c,d) = (vs1 U vs2) with the tuple (1,min,max,0)
        member this.Join states1 states2 = 

            let alocs1 = states1.ALocs
            let alocs2 = states2.ALocs
            let alocs = new Set<aloc>(alocs1)+new Set<aloc>(alocs2)

            // aloc*VS list from which will be built the new map
            let mutable list = []
            for aloc in alocs do
                // symbolic value for ⊥/⊤
                if aloc="0" then ignore()
                else

                    // if the current aloc is not contained in one of the two States, 
                    // then we take the other one element; 
                    // that is <aloc-i,VS-i> U <null> = <aloc-i,VS-i> U <aloc-i,⊥> = <aloc-i,VS-i>
                    // goon is a flag that indicate if continue with the current computation 
                    // or go to the next one because an exception has been raised
                    let mutable goon = true
                    let mutable vs1 = new ValueSet([])
                    let mutable vs2 = new ValueSet([])
                    try
                        vs1 <- states1.Map.[aloc]
                    with | :? System.Collections.Generic.KeyNotFoundException -> 
                          list <- list@[(aloc,states2.Map.[aloc])]; goon <- false
                    try
                        vs2 <- states2.Map.[aloc]
                    with | :? System.Collections.Generic.KeyNotFoundException -> 
                          list <- list@[(aloc,states1.Map.[aloc])]; goon <- false

                    // no exception has been raised
                    if goon then

                        // set of keys
                        let newVs = new ValueSet([])
                        let s1 = new Set<MemoryRegion>(vs1.MemRegs())
                        let s2 = new Set<MemoryRegion>(vs2.MemRegs())
                        let set = Set.toList (s1+s2)

                        // scan keys
                        let rec f (keys : MemoryRegion list) = 
                            match keys with
                            |[] -> list <- list@[(aloc,newVs)]
                            |k::ks -> 
                                    // if key.ID = -1 -> symbolic value, had to be wasted
                                    if k.ID() = -1 then f ks
                                    else

                                        // if s1.Contains(k) and an exception is raised -> vs2.[k] = ⊥
                                        if s1.Contains(k) then
                                             try
                                                 let newSet = vs1.VS.[k]+vs2.VS.[k]
                                                 newVs.Add(k,(1,(Set.minElement newSet),(Set.maxElement newSet),0)); f ks
                                                
                                             with 
                                             | :? System.Collections.Generic.KeyNotFoundException -> 
                                              newVs.Add(k,(1,(Set.minElement vs1.VS.[k]),(Set.maxElement vs1.VS.[k]),0)); f ks
    
                                        // else, if an exception is raised -> vs1.[k] = ⊥
                                        else try 
                                                 let newSet = vs1.VS.[k]+vs2.VS.[k]
                                                 newVs.Add(k,(1,(Set.minElement newSet),(Set.maxElement newSet),0)); f ks
                                             with 
                                             | :? System.Collections.Generic.KeyNotFoundException -> 
                                              newVs.Add(k,(1,(Set.minElement vs2.VS.[k]),(Set.maxElement vs2.VS.[k]),0)); f ks
                                          
                        in f set

                    // go to the next aloc
                    else ignore()
            new AbstractState(list)

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

    // states ABC const = (a,b,c,d) -> (a+const, b, c, d+const) 
    //for each RIC (a,b,c,d) in vs, for each vs in states
    member this.AdjustByConstant (states : AbstractState) (cnst : int) = 
    
        let mutable list = [] 
        for aloc in states.ALocs do

            let vs = states.Map.[aloc]
            let newVS = new ValueSet([])

            let rec f (tuples : (MemoryRegion * (int * int * int * int)) list ) = 
                match tuples with
                |[] -> newVS
                |(x,(a,b,c,d))::xs -> newVS.Add(x,((a+cnst),b,c,(d+cnst))) ; f xs 
            in list <- list@[(aloc, (f (vs.Tuples())))]
        list
;;
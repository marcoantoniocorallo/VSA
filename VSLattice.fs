// "The following operators are defined for value-sets. 
//  All operators are pointwise applications of the corresponding RIC operator"
// Comportamento previsto: 
// vs1 < vs2 SSE vs1[i] ⊆ vs2[i] per i=0,...,vs.Size (! Potrebbero essere inconfrontabili !)
// vs1 JOIN vs2 -> RICvs1-i JOIN RICvs2-i per i=0,...,vs.Size
// vs1 MEET vs2 -> RICvs1-i MEET RICvs2-i per i=0,...,vs.Size
// Nel caso in cui uno dei due oggetti non contenga un elemento (Heapi, ARi), viene considerato l'elemento Bottom
open FSharp.Collections

type VSLattice() =

    interface ILattice<ValueSet> with

        // Bottom element = ({},...,{})
        member this.Bot() = new ValueSet("bottom")
        
        // Top element = ( R,....,R )
        member this.Top() = new ValueSet("top")
        
        // Returns true if the value-set vs1 is a subset of vs2, false otherwise
        member this.Leq vs1 vs2 =            
    
            // vs1's list of keys
            let keys1 = vs1.MemRegs
            let rec f keys = 
                match keys with
                |[] -> true
                |k::ks -> try 
                             if not(vs1.VS.[k].IsSubsetOf(vs2.VS.[k])) then false
                                                                       else f ks
                          // Memory-region in vs1 assente in vs2  
                          with | :? System.Collections.Generic.KeyNotFoundException -> false
            in f keys1
                    
        // Returns the union (join) of value-sets vs1 and vs2
        // if exist i : i ∈ vs1 ^ i ∉ vs2 -> ( vs1[i] U vs2[i] = vs1[i] U ⊥ ) and vice versa
        // Note: Over-approxime the tuple (a,b,c,d) = (vs1 U vs2) with the tuple (1,min,max,0)
        member this.Join vs1 vs2 = 
            
            // set of keys
            let newVs = new ValueSet([])
            let s1 = new Set<MemoryRegion>(vs1.MemRegs)
            let s2 = new Set<MemoryRegion>(vs2.MemRegs)
            let set = Set.toList (s1+s2)

            // scan keys
            let rec f keys = 
                match keys with
                |[] -> newVs
                |k::ks -> 
                        // if s1.Contains(k) and an exception is raised -> vs2.[k] = ⊥
                        if s1.Contains(k) 
                            then try
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

    // vs ABC const = (a,b,c,d) -> (a+const, b, c, d+const) for each RIC (a,b,c,d) in vs
    member this.AdjustByConstant (vs : ValueSet) (cnst : int) = 
        let newVS = new ValueSet([])
        let rec f (tuples : (MemoryRegion * (int * int * int * int)) list ) = 
            match tuples with
            |[] -> newVS
            |(x,(a,b,c,d))::xs -> newVS.Add(x,((a+cnst),b,c,(d+cnst))) ; f xs 
        in f (vs.Tuples())
;;
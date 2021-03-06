(* Transfer functions : Exp -> (AbstractState -> AbstractState)
 *
 * GlobalDec : R1, R2,..., Rn ;; HeapDec : R1, R2,..., Rn
 *     e.After := e.Before ∪ [R1 -> ⊤] ∪ [R2 -> ⊤] ∪ ... ∪ [Rn -> ⊤]
 * 
 * SimpleAss : R1 = k ;; SimpleHAss : R1 = k
 *    e.After := e.Before \ {R1} ∪ [R1 -> k]
 *
 * Ass1 : R1 = R2 + c 
 *    let [R2 -> vs] ∈ e.Before
 *    e.After := e.Before \ [R1 -> *] ∪ [R1 -> vs AdjustByConst c]
 *
 * LeqConst : R1 ≤ c
 *    let [R1 -> vs1] ∈ e.Before and vsc = ([−∞, c], ⊤, ..., ⊤)
 *    e.After := e.Before − [R1 -> *] ∪ [R1 -> vs1 MEET vsc ]
 *
 * GeqConst : R1 ≥ c
 *    let [R1 -> vs1] ∈ e.Before and vsc = ([c, ∞], ⊤, ..., ⊤)
 *    e.After := e.Before − [R1 -> *] ∪ [R1 -> vs1 MEET vsc ]
 *
 * LeqVar : R1 ≤ R2
 *    let [R1 -> vs1 ], [R2 -> vs2] ∈ e.Before and vslb = RemoveUpperBounds(vs2)
 *    e.After := e.Before − [R1 -> ∗] ∪ [R1 -> vs1 MEET vslb]
 *
 *)

let TransferFunctions(e : Exp) : (AbstractState -> AbstractState) = 

    // Lattice instance for using ILattice methods (join,meet,...)
    let L = new ValueSet(Bottom)

    match e with

    |HeapDec(alocs) -> (fun (x : AbstractState) -> x)   // I

    |SimpleHAss(aloc,n) -> (fun (x : AbstractState) -> x)   // I
                         
    |GlobalDec(alocs) -> 

        // define AbstractState of <aloc, ⊤> for each aloc in the exp
        // note: Global mem-reg = ⊤, the other ones = ⊥
        let abs = new AbstractState() in
        let vars = alocs |> List.map (fun x -> abs.Add(x,new ValueSet(new MemoryRegion(RegionType.Global,1),Top))) in
        (fun (x : AbstractState) -> (L:>ILattice<AbstractState>).Join x abs)

    |SimpleAss(aloc,n) -> // returns fun : x -> x[n/x.[aloc].Global] 
        
        let f (oldAbs : AbstractState) =
        
            // must create a clone of the old abstract state
            let abs = new AbstractState()
            let ks = oldAbs.Keys |> Seq.toList
            for k in ks do
                let memregs = oldAbs.[k].MemRegs()
                let tuples = oldAbs.[k].Tuples.Values |> Seq.toList
                let l = List.zip memregs tuples
                abs.Add(k,new ValueSet(l))

            // subst in clone.Global the value n
            abs.[aloc].Add(new MemoryRegion(RegionType.Global,1),Ric(1,n,n,0))
            abs 
        
        in (fun (x : AbstractState) -> f x )

    |Ass1(aloc1,aloc2,cnst) -> (fun x -> L.AdjustByC x aloc1 aloc2 cnst)

    |LeqConst(aloc,c) -> // returns fun : x -> (x \ [R1 -> *] u [R1 -> vs MEET [-inf, c]])
        
        let f (oldAbs : AbstractState) =
        
            // ⊤ MEET [-inf,c] = [-inf,c] but is hard to compute, 
            // then returns ⊤: not very accurate but sound 
            if oldAbs.[aloc].Tuples.[MemoryRegion(RegionType.Global,1)] = Top then oldAbs
            else

                // must create a clone of the old abstract state
                let abs = new AbstractState()
                let ks = oldAbs.Keys |> Seq.toList
                for k in ks do
                    let memregs = oldAbs.[k].MemRegs()
                    let tuples = oldAbs.[k].Tuples.Values |> Seq.toList
                    let l = List.zip memregs tuples
                    abs.Add(k,new ValueSet(l))

                // subst in clone.Global the value (oldRIC MEET [-inf, c])
                let values = oldAbs.[aloc].Sets.[MemoryRegion(RegionType.Global,1)] |> Set.toList

                                                // set = values MEET [-inf, c]
                let set = new Set<int>(values |> List.filter (fun x -> if x<=c then true else false))
                abs.[aloc].Add(new MemoryRegion(RegionType.Global,1), set)
                abs

        in (fun (x : AbstractState) -> f x )
        
    |GeqConst(aloc,c) -> // returns fun : x -> (x \ [R1 -> *] u [R1 -> vs MEET [c, inf]])
        
        let f (oldAbs : AbstractState) =

            // ⊤ MEET [c,inf] = [c,inf] but is hard to compute, 
            // then returns ⊤: not very accurate but sound 
            if oldAbs.[aloc].Tuples.[MemoryRegion(RegionType.Global,1)] = Top then oldAbs
            else
        
                // must create a clone of the old abstract state
                let abs = new AbstractState()
                let ks = oldAbs.Keys |> Seq.toList
                for k in ks do
                    let memregs = oldAbs.[k].MemRegs()
                    let tuples = oldAbs.[k].Tuples.Values |> Seq.toList
                    let l = List.zip memregs tuples
                    abs.Add(k,new ValueSet(l))
            
                // subst in clone.Global the value (oldRIC MEET [c, inf])
                let values = oldAbs.[aloc].Sets.[MemoryRegion(RegionType.Global,1)] |> Set.toList

                                            // set = values MEET [c, inf]
                let set = new Set<int>(values |> List.filter (fun x -> if x>=c then true else false))
                abs.[aloc].Add(new MemoryRegion(RegionType.Global,1), set)
                abs

        in (fun (x : AbstractState) -> f x )

    |LeqVar(R1,R2) -> // returns fun : x -> (x \ [R1 -> *] u [R1 -> vs1 MEET RemoveUpperBound(vs2)])

        let f (oldAbs : AbstractState) = 

            // must create a clone of the old abstract state
            let abs = new AbstractState()
            let ks = oldAbs.Keys |> Seq.toList
            for k in ks do
                let memregs = oldAbs.[k].MemRegs()
                let tuples = oldAbs.[k].Tuples.Values |> Seq.toList
                let l = List.zip memregs tuples
                abs.Add(k,new ValueSet(l))

            // abs.R1.[mr] <- abs.[R1].[mr] MEET vslb.[mr] for each mr
            for mr in (abs.[R1].MemRegs()) do

                match (abs.[R1].IsTopOf(mr),abs.[R1].IsBotOf(mr),abs.[R2].IsTopOf(mr),abs.[R2].IsBotOf(mr)) with
                
                // ⊤ MEET ⊤ = ⊤ ; VS MEET ⊤ = VS ; ⊤ MEET RmUppBound(VS) = ⊤ (not very accurate, but sound!) 
                |(true,_,_,false)
                |(false,false,true,_) -> ignore()
                
                // VS MEET ⊥ = ⊥
                |(false,false,false,true) -> abs.[R1].Add(mr,abs.[R2].Tuples.[mr])

                |(_,_,_,_) ->

                    let values1 = oldAbs.[R1].Sets.[mr] |> Set.toList
                    let values2 = oldAbs.[R2].Sets.[mr]
                    let set = new Set<int>(values1 |> List.filter 
                                  (fun x -> (Set.contains x values2) || (x > Set.maxElement values2) ) )
                    abs.[R1].Add(mr,set)
        
            abs

        in (fun (x : AbstractState) -> f x)

    // TODO: Other cases
    |_ -> (fun (x : AbstractState) -> x)
;;

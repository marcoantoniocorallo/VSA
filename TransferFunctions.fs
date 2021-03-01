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
            let set = new Set<int>(values |> List.filter (fun x -> if x<=c then true else false))
            abs.[aloc].Add(new MemoryRegion(RegionType.Global,1), set)
            abs

        in (fun (x : AbstractState) -> f x )
        
    |GeqConst(aloc,c) -> // returns fun : x -> (x \ [R1 -> *] u [R1 -> vs MEET [c, inf]])
        
        let f (oldAbs : AbstractState) =
        
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
            let set = new Set<int>(values |> List.filter (fun x -> if x>=c then true else false))
            abs.[aloc].Add(new MemoryRegion(RegionType.Global,1), set)
            abs

        in (fun (x : AbstractState) -> f x )

    // TODO: Other cases
    |_ -> (fun (x : AbstractState) -> x)
;;

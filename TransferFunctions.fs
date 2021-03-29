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
 *    let [R1 -> vs1] ∈ e.Before and vsc = ([−inf, c], ⊤, ..., ⊤)
 *    e.After := e.Before − [R1 -> *] ∪ [R1 -> vs1 MEET vsc ]
 *
 * GeqConst : R1 ≥ c
 *    let [R1 -> vs1] ∈ e.Before and vsc = ([c, inf], ⊤, ..., ⊤)
 *    e.After := e.Before − [R1 -> *] ∪ [R1 -> vs1 MEET vsc ]
 *
 * LeqVar : R1 ≤ R2
 *    let [R1 -> vs1 ], [R2 -> vs2] ∈ e.Before and vslb = RemoveUpperBounds(vs2)
 *    e.After := e.Before − [R1 -> ∗] ∪ [R1 -> vs1 MEET vslb]
 *
 *)

// Macro def about the main memregs, so they can be edited
let AR0 = MemoryRegion(RegionType.AR,0);; // Main procedure AR
let GLOBAL = MemoryRegion(RegionType.Global,1);; // Unique Global MR

let TransferFunctions(e : Exp) : (AbstractState -> AbstractState) = 

    // Lattice instance for using ILattice methods (join,meet,...)
    let VS = new ValueSet(Bottom)
    let L = (VS:>ILattice<AbstractState>)

    match e with

    |HeapDec(alocs) -> (fun (x : AbstractState) -> x)   // I

    |SimpleHAss(aloc,n) -> (fun (x : AbstractState) -> x)   // I
                         
    |GlobalDec(alocs) -> 

        // define AbstractState of <aloc, ⊤> for each aloc in the exp
        // note: Global mem-reg = ⊤, the other ones = ⊥
        let abs = new AbstractState() in
        let vars = alocs 
                   |> List.map (fun x -> abs.Add(x,new ValueSet(GLOBAL,Top) ) )
        in (fun (x : AbstractState) -> L.Join x abs)

    |SimpleAss(aloc,n) -> // returns fun : x -> x[n/x.[aloc].Global] 
        
        let f (oldAbs : AbstractState) =
        
            // must create a clone of the old abstract state
            let abs = new AbstractState()
            let ks = oldAbs.Keys |> Seq.toList
            for k in ks do
                abs.Add(k,oldAbs.[k].Clone())

            // subst in clone.Global the value n
            abs.[aloc].AddChange(GLOBAL,Interval(Int(n),Int(n)))
            abs 
        
        in (fun x -> f x )

    |Ass1(aloc1,aloc2,cnst) -> 
        
        let f (oldAbs : AbstractState) =
            let newAbs = VS.AdjustByC oldAbs aloc2 cnst
            newAbs.[aloc1] <- newAbs.[aloc2].Clone()
            newAbs.[aloc2] <- oldAbs.[aloc2].Clone()
            newAbs

        in (fun x -> f x)

    |LeqConst(aloc,c) -> // returns fun : x -> (x \ [R1 -> *] u [R1 -> vs MEET [-inf, c]])
        
        let f (oldAbs : AbstractState) =

            // top = ([-inf,c],⊤,...,⊤)
            let top = L.Top()
            top.[aloc].AddChange(GLOBAL,Interval(NegativeInf,Int(c)))
            L.Meet oldAbs top

        in (fun x -> f x)
        
    |GeqConst(aloc,c) -> // returns fun : x -> (x \ [R1 -> *] u [R1 -> vs MEET [c, inf]])
        
        let f (oldAbs : AbstractState) =

            // top = ([c,inf],⊤,...,⊤)
            let top = L.Top()
            top.[aloc].AddChange(GLOBAL,Interval(Int(c),PositiveInf))
            L.Meet oldAbs top
            
        in (fun x -> f x)

    |LeqVar(R1,R2) -> // returns fun : x -> (x \ [R1 -> *] u [R1 -> vs1 MEET RemoveUpperBound(vs2)])

        let f (oldAbs : AbstractState) = 
           
             // must create a clone of the old abstract state
            let abs = new AbstractState()
            let ks = oldAbs.Keys |> Seq.toList
            for k in ks do
                abs.Add(k,oldAbs.[k].Clone())
            
            // abs.[R1] = abs.[R2].RmUpperBounds ; 
            abs.[R1] <- abs.[R2].Clone()
            abs.[R1].RmUpperBounds()
            L.Meet oldAbs abs

        in (fun (x : AbstractState) -> f x)

    // TODO: Other cases
    |_ -> (fun (x : AbstractState) -> x)
;;

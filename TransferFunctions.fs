(* Transfer functions : Exp -> (AbstractState -> AbstractState)
 *
 * GlobalDec : R1, R2,..., Rn ;; HeapDec : R1, R2,..., Rn
 *     e.After := e.Before ∪ [R1 -> ⊤] ∪ [R2 -> ⊤] ∪ ... ∪ [Rn -> ⊤]
 * 
 * SimpleAss : R1 = k ;; SimpleHAss : R1 = k
 *    e.After := e.Before \ {R1} ∪ [R1 -> k]
 *
 * SumConst : R1 = R2 + c 
 *    let [R2 -> vs] ∈ e.Before
 *    e.After := e.Before \ [R1 -> *] ∪ [R1 -> vs AdjustByConst c]
 *
 * TimesConst : R1 = R2 + c 
 *    let [R2 -> vs] ∈ e.Before
 *    e.After := e.Before \ [R1 -> *] ∪ [R1 -> vs TimesByConst c]
 *
 * Ass2 : *(R1+c1) = R2+c2
 *    let [R1 -> VS2], [R2 -> VS2] ∈ e.Before 
 *        and (F,P) = *((VS1 AdjustByConst c1), s)
 *        and tmp = e.Before \ {[a -> *] | a ∈ P ∪ F} ∪ {[p -> ⊤] | p ∈ P}
 *        and Proc be the procedure containing the statement
 *    if (|F| = 1 and |P| = 0 and (Proc is not recursive) and (F has not heap objects)
 *        // Strong update
 *        then e.After := tmp ∪ {[v -> (VS2 AdjustByConst c2)] | v ∈ F}
 *        // Weak update
 *        else e.After := tmp ∪ {[v -> (VS2 AdjustByConst c2) JOIN VSv ] | v ∈ F, [v -> VSv] ∈ e.Before}
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
            abs.SetNWidened(oldAbs.GetNWidened())
            let ks = oldAbs.Keys |> Seq.toList
            for k in ks do
                abs.Add(k,oldAbs.Map.[k].Clone())

            // subst in clone.Global the value n
            abs.Map.[aloc].AddChange(GLOBAL,Interval(Int(n),Int(n)))
            abs 
        
        in f

    |SumConst(aloc1,aloc2,cnst) -> // returns fun : x -> (x \ [R1 -> *] u [R1 -> vs2 AdjustByConst c])
        
        let f (oldAbs : AbstractState) =
            let newAbs = VS.AdjustByC oldAbs aloc2 cnst
            newAbs.Map.[aloc1] <- newAbs.Map.[aloc2].Clone()
            newAbs.Map.[aloc2] <- oldAbs.Map.[aloc2].Clone()
            newAbs

        let g (oldAbs : AbstractState) = VS.AdjustByC oldAbs aloc1 cnst

        in if aloc1<>aloc2 then f else g

    |TimesConst(aloc1,aloc2,cnst) -> // returns fun : x -> (x \ [R1 -> *] u [R1 -> vs2 TimesByConst c])
        
        let f (oldAbs : AbstractState) =
            let newAbs = VS.TimesByC oldAbs aloc2 cnst
            newAbs.Map.[aloc1] <- newAbs.Map.[aloc2].Clone()
            newAbs.Map.[aloc2] <- oldAbs.Map.[aloc2].Clone()
            newAbs
        let g (oldAbs : AbstractState) = VS.TimesByC oldAbs aloc1 cnst
        in if aloc1<>aloc2 then f else g

    |LeqConst(aloc,c) -> // returns fun : x -> (x \ [R1 -> *] u [R1 -> vs MEET [-inf, c]])
        
        let f (oldAbs : AbstractState) =

            // top = ([-inf,c],⊤,...,⊤)
            let top = L.Top()
            top.Map.[aloc].AddChange(GLOBAL,Interval(NegativeInf,Int(c)))
            L.Meet oldAbs top

        in f
        
    |GeqConst(aloc,c) -> // returns fun : x -> (x \ [R1 -> *] u [R1 -> vs MEET [c, inf]])
        
        let f (oldAbs : AbstractState) =

            // top = ([c,inf],⊤,...,⊤)
            let top = L.Top()
            top.Map.[aloc].AddChange(GLOBAL,Interval(Int(c),PositiveInf))
            L.Meet oldAbs top
            
        in f

    |LeqVar(R1,R2) -> // returns fun : x -> (x \ [R1 -> *] u [R1 -> vs1 MEET RemoveUpperBound(vs2)])

        let f (oldAbs : AbstractState) = 
           
             // must create a clone of the old abstract state
            let abs = new AbstractState()
            let ks = oldAbs.Keys |> Seq.toList
            for k in ks do
                abs.Add(k,oldAbs.Map.[k].Clone())
            
            // abs.[R1] = abs.[R2].RmUpperBounds ; 
            abs.Map.[R1] <- abs.Map.[R2].Clone()
            abs.Map.[R1].RmUpperBounds()
            L.Meet oldAbs abs

        in f

    // TODO: Other cases (Heap dec and assignments, structs,... )
    |_ -> (fun (x : AbstractState) -> x)
;;

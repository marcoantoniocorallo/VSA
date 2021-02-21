(* Transfer functions : Exp -> (AbstractState -> AbstractState)
 *
 * GlobalDec : R1, R2,..., Rn / HeapDec : R1, R2,..., Rn
 * e.After := e.Before ∪ [R1 -> ⊤] ∪ [R2 -> ⊤] ∪ ... ∪ [Rn -> ⊤]
 * 
 * SimpleAss : R1 = k / SimpleHAss : R1 = k
 * e.After := e.Before \ {R1} ∪ [R1 -> k]
 *)

let TransferFunctions(e : Exp) : (AbstractState -> AbstractState) = 

    // Lattice instance for using ILattice methods (join,meet,...)
    let L = new ValueSet("bot")

    match e with

    |HeapDec(alocs) -> (fun (x : AbstractState) -> x)   // I

    |SimpleHAss(aloc,n) -> (fun (x : AbstractState) -> x)   // I
                         
    |GlobalDec(alocs) -> 

        // define AbstractState of <aloc, ⊤> for each aloc in the exp
        // note: Global mem-reg = ⊤, the other ones = ⊥
        let abs = new AbstractState() in
        let vars = alocs |> List.map (fun x -> abs.Add(x,new ValueSet(new MemoryRegion(RegionType.Global,1),"top"))) in
        (fun (x : AbstractState) -> (L:>ILattice<AbstractState>).Join x abs)

    |SimpleAss(aloc,n) -> // returns fun : x -> x[n/aloc] 
        
        let f (oldAbs : AbstractState) =
        
            // must create a clone of the old abstract state
            let abs = new AbstractState()
            let ks = oldAbs.Keys |> Seq.toList
            for k in ks do
                let memregs = oldAbs.[k].MemRegs()
                let tuples = oldAbs.[k].Tuples.Values |> Seq.toList
                let l = List.zip memregs tuples
                abs.Add(k,new ValueSet(l))

            // subst clone.Global[n/aloc]
            abs.[aloc].Add(new MemoryRegion(RegionType.Global,1),(1,n,n,0))
            abs 
        
        in (fun (x : AbstractState) -> f x )

    // TODO: Other cases
    |_ -> (fun (x : AbstractState) -> x)
;;
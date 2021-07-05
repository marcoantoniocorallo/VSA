(* Transfer functions : Exp -> (AbstractState -> AbstractState)
 *
 * GlobalDec : R1, R2,..., Rn ;; HeapDec : R1, R2,..., Rn
 *     e.After := e.Before ∪ [R1 -> ⊤] ∪ [R2 -> ⊤] ∪ ... ∪ [Rn -> ⊤]
 * 
 * SimpleAss : R1 = k ;; SimpleHAss : R1 = k
 *    e.After := e.Before \ {R1} ∪ [R1 -> k]
 * 
 * Array : A[0...size-1] = k
 *    e.After := e.Before ∪ [A+0 -> k] ∪ [A+4 -> k] ... ∪ [A+(s-4) -> k]
 *
 * ArrayAss : A[i] = k
 *    e.After := e.Before \ {A+j} ∪ [A+j -> k] for j ∈ i.VS 
 *
 * SumConst : R1 = R2 + c 
 *    let [R2 -> vs] ∈ e.Before
 *    e.After := e.Before \ [R1 -> *] ∪ [R1 -> vs AdjustByConst c]
 *
 * TimesConst : R1 = R2 + c 
 *    let [R2 -> vs] ∈ e.Before
 *    e.After := e.Before \ [R1 -> *] ∪ [R1 -> vs TimesByConst c]
 *
 * SumAloc : R1 = R2 + R3
 *    let [R2 -> vs2], [R3 -> vs3] ∈ e.Before
 *    e.After := e.Before \ [R1 -> *] ∪ [R1 -> vs2 + vs3]
 *
 * TimesAloc : R1 = R2 * R3
 *    let [R2 -> vs2], [R3 -> vs3] ∈ e.Before
 *    e.After := e.Before \ [R1 -> *] ∪ [R1 -> vs2 * vs3]
 *
 * LeqConst : R1 ≤ c
 *    let [R1 -> vs1] ∈ e.Before and vsc = ([−inf, c], ⊤, ..., ⊤)
 *    e.After := e.Before − [R1 -> *] ∪ [R1 -> vs1 MEET vsc ]
 *
 * GeqConst : R1 ≥ c
 *    let [R1 -> vs1] ∈ e.Before and vsc = ([c, inf], ⊤, ..., ⊤)
 *    e.After := e.Before − [R1 -> *] ∪ [R1 -> vs1 MEET vsc ]
 *
 * GeqAloc : R1 ≥ R2
 *    let [R1 -> vs1 ], [R2 -> vs2] ∈ e.Before 
 *    let vslb = RemoveUpperBounds(vs2) and vsub = RemoveLowerBounds(vs1)
 *    e.After := e.Before − [R1 -> ∗] ∪ [R1 -> vs1 MEET vslb] - [R2 -> ∗] ∪ [R2 -> vs2 MEET vsub]
 *
 *)

// Macro def about the main memregs, so they can be edited
let AR0 = MemoryRegion(RegionType.AR,0);; // Main procedure AR
let GLOBAL = MemoryRegion(RegionType.Global,1);; // Unique Global MR
let MaxGap = 100;;

let rec TransferFunctions(e : Exp) : (AbstractState -> AbstractState) = 

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
            let abs = oldAbs.Clone()

            // subst in clone.Global the value n
            abs.ChangeValue(aloc,GLOBAL,Interval(Int(n),Int(n)))
            abs 
        in f

    |Array(A,size,n) -> // returns fun : x -> x u [A[0,...,sizeOfA] -> ⊤])
        let abs = new AbstractState() in
        for i=0 to size-1 do
            if i%4=0 then abs.Add(A+"+"+(string i),new ValueSet(GLOBAL,Interval(Int(n),Int(n)))) 
            else ignore()
        (fun (x : AbstractState) -> L.Join x abs)

    |ArrayAss(A,i,k) -> // returns fun : x -> x[k.Global/A[i].Global]

        let intervalJoin i1 i2 = 
           match i1, i2 with
           |Top,_ 
           |_,Top -> Top
           |Bottom,i -> i 
           |i,Bottom -> i
           |Interval(a,b),Interval(c,d) -> Interval(min a c, max b d)

        let f (oldAbs : AbstractState) =
            let newAbs = oldAbs.Clone()
            for mr in oldAbs.Get(i).MemRegs() do
                match oldAbs.Get(i).ValuesIn(mr) with
                |Interval(Int(l),Int(r)) when (r-l)<=MaxGap ->
                    for j=l to r do
                        try 
                            newAbs.ChangeValue( A+"+"+(string (j*4)), mr, 
                                intervalJoin (oldAbs.Get(k).ValuesIn(mr)) (oldAbs.Get(A+"+"+(string (j*4))).ValuesIn(mr)) 
                            )
                        with
                        | :?System.Collections.Generic.KeyNotFoundException ->
                        newAbs.Add(A+"+"+(string (j*4)), oldAbs.Get(k))

                |Interval(l,r) -> failwith(A+"["+(Interval(l,r).ToString())+"]:\nPossible Stack Overflow")
                |Top -> failwith(A+"[Top]:\nPossible Stack Overflow")
                |Bottom -> for j=0 to (SizeOf.[A]/4)-1 do newAbs.ChangeValue( A+"+"+(string (j*4)), mr, Bottom)
            newAbs
        in f

    (*
    |ArrayLeqConst(A,i,c) ->

        let f (oldAbs : AbstractState) = 
            let mutable newAbs = oldAbs.Clone()
            for mr in oldAbs.Get(i).MemRegs() do
                match oldAbs.Get(i).ValuesIn(mr) with
                |Interval(Int(l),Int(r)) when (r-l)<=MaxGap -> 
                    for j=l to r do
                        try 
                            newAbs <- (TransferFunctions(LeqConst(A+"+"+(string (j*4)),c)) newAbs)
                        with
                        | :?System.Collections.Generic.KeyNotFoundException ->
                        newAbs.Add(A+"+"+(string (j*4)), new ValueSet(Interval(NegativeInf,Int(c))))

                |Interval(l,r) -> failwith(A+"["+(Interval(l,r).ToString())+"]:\nPossible Stack Overflow")
                |Top -> failwith(A+"[Top]:\nPossible Stack Overflow")
                |Bottom -> for j=0 to (SizeOf.[A]/4)-1 do newAbs.ChangeValue(A+"+"+(string (j*4)), mr, Bottom)
            newAbs
        in f

    |ArrayGeqConst(A,i,c) ->

        let f (oldAbs : AbstractState) = 
            let mutable newAbs = oldAbs.Clone()
            for mr in oldAbs.Get(i).MemRegs() do
                match oldAbs.Get(i).ValuesIn(mr) with
                |Interval(Int(l),Int(r)) when (r-l)<=MaxGap -> 
                    for j=l to r do
                        try 
                            newAbs <- (TransferFunctions(GeqConst(A+"+"+(string (j*4)),c)) newAbs)
                        with
                        | :?System.Collections.Generic.KeyNotFoundException ->
                        newAbs.Add(A+"+"+(string (j*4)), new ValueSet(Interval(Int(c),PositiveInf)))

                |Interval(l,r) -> failwith(A+"["+(Interval(l,r).ToString())+"]:\nPossible Stack Overflow")
                |Top -> failwith(A+"[Top]:\nPossible Stack Overflow")
                |Bottom -> for j=0 to (SizeOf.[A]/4)-1 do newAbs.ChangeValue(A+"+"+(string (j*4)), mr, Bottom)
            newAbs
        in f
    *)
    
    |SumConst(aloc1,aloc2,cnst) -> // returns fun : x -> (x \ [R1 -> *] u [R1 -> vs2 AdjustByConst c])

        let f (oldAbs : AbstractState) =
            let newAbs = VS.AdjustByC oldAbs aloc2 cnst
            newAbs.Set(aloc1,newAbs.Get(aloc2))
            newAbs.Set(aloc2,oldAbs.Get(aloc2))
            newAbs

        let g (oldAbs : AbstractState) = VS.AdjustByC oldAbs aloc1 cnst
        in if aloc1<>aloc2 then f else g

    |TimesConst(aloc1,aloc2,cnst) -> // returns fun : x -> (x \ [R1 -> *] u [R1 -> vs2 TimesByConst c])
        
        let f (oldAbs : AbstractState) =
            let newAbs = VS.TimesByC oldAbs aloc2 cnst
            newAbs.Set(aloc1,newAbs.Get(aloc2))
            newAbs.Set(aloc2,oldAbs.Get(aloc2))
            newAbs
        let g (oldAbs : AbstractState) = VS.TimesByC oldAbs aloc1 cnst
        in if aloc1<>aloc2 then f else g

    |SumAloc(aloc1,aloc2,aloc3) -> // returns fun : x -> (x \ [R1 -> *] u [R1 -> vs2 + vs3])
        
        let f (oldAbs : AbstractState) =
            let newVS = oldAbs.Get(aloc2) + oldAbs.Get(aloc3)
            let abs = oldAbs.Clone()
            abs.Set(aloc1,newVS)
            abs
        in f

    |TimesAloc(aloc1, aloc2, aloc3) -> // returns fun : x -> (x \ [R1 -> *] u [R1 -> vs2 * vs3])
        
        let f (oldAbs : AbstractState) =
            let newVS = oldAbs.Get(aloc2) * oldAbs.Get(aloc3)
            let abs = oldAbs.Clone()
            abs.Set(aloc1,newVS)
            abs
        in f

    |LeqConst(aloc,c) -> // returns fun : x -> (x \ [R1 -> *] u [R1 -> vs MEET [-inf, c]])
        
        let f (oldAbs : AbstractState) =

            // top = ([-inf,c],⊤,...,⊤)
            let top = L.Join (L.Top()) oldAbs
            top.ChangeValue(aloc,GLOBAL,Interval(NegativeInf,Int(c)))
            L.Meet oldAbs top

        in f
        
    |GeqConst(aloc,c) -> // returns fun : x -> (x \ [R1 -> *] u [R1 -> vs MEET [c, inf]])

        let f (oldAbs : AbstractState) =
            // top = ([c,inf],⊤,...,⊤)
            let top = L.Join (L.Top()) oldAbs
            top.ChangeValue(aloc,GLOBAL,Interval(Int(c),PositiveInf))
            L.Meet oldAbs top 
            
        in f

    |GeqAloc(R1,R2) -> 
    // returns fun : x -> (x \ [R1 -> *] u [R1 -> vs1 MEET RemoveUpperBound(vs2)] 
    //                       \ [R2 -> *] u [R2 -> vs2 MEET RemoveLowerBound(vs1)])

        let f (oldAbs : AbstractState) = 
            let abs = oldAbs.Clone()
            
            // abs.[R1] = abs.[R2].RmUpperBounds ; 
            abs.Set(R1,abs.Get(R2))
            abs.RmUp(R1)

            // abs.[R2] = abs.[R1].RmLowerBounds ; 
            abs.Set(R2,abs.Get(R1))
            abs.RmLow(R2)

            L.Meet oldAbs abs

        in f

    // TODO: Other cases (Heap dec and assignments, structs,... )
    |_ -> (fun (x : AbstractState) -> x)
;;
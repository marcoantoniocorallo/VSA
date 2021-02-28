// list of all MRs and them ID, in order to initialize them value with ⊥
// if the MRs list will become dynamic in the future (AR1, AR2,... ARi,..., ARn; Heap1,..., Heapi,... Heapn),
// then we will simply do a preliminary analysis to become aware of all possible memory regions.
let availableMRs = [new MemoryRegion(RegionType.AR,0);new MemoryRegion(RegionType.Global,1);] ;;

// list of every defined var
let availableVarsAnalysis (cfg : ICFG) : (aloc list) =
    let entryNode = cfg.EntryBlock()
    let list = entryNode.Succ()

    // if statm = {HeapDec/GlobalDec} -> add new defined vars to ls
    let h statm ls = match statm with |GlobalDec(l) |HeapDec(l) -> ls@l |_ -> ls

    let rec f succ res = 
        match succ with
        |[] -> res
        |x::xs -> 
                let rec g (deepSucc : INode list) k = 
                    match deepSucc with
                    |[] -> f xs k 
                    |y::ys -> g ys (h (y.Statm()) (k@(f (y.Succ()) [])) )

                in g (x.Succ()) (h (x.Statm()) res)

    in f list (h (entryNode.Statm()) [])
    |> List.distinct ;;
let mutable availableVars : aloc list = []
(* Other todo analysis: #Malloc for counting #HeapRegs; #Procs for counting #ARRegs *)
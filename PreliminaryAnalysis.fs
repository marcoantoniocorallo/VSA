open System.Collections.Generic;; 

// Some mutable global vars are defined into preliminary analysis, in that they're used in some routines;
// because of the static binding they cannot be redefined, so they're mutable.
// The values will be overwritten in Main routine.

// list of all MRs and them ID, in order to initialize them value with ⊥
// if the MRs list will become dynamic in the future (AR1, AR2,... ARi,..., ARn; Heap1,..., Heapi,... Heapn),
// then we will simply do a preliminary analysis to become aware of all possible memory regions.
let availableMRs = [new MemoryRegion(RegionType.AR,0);new MemoryRegion(RegionType.Global,1);] ;;

// list of every defined var
let mutable availableVars : aloc list = [] ;;  
let availableVarsAnalysis (cfg : ICFG) : (aloc list) =

    // if statm = {HeapDec/GlobalDec} -> add new defined vars to ls
    let h statm ls = match statm with |GlobalDec(l) -> ls@l |HeapDec(l) -> ls@(List.map fst l) |_ -> ls

    let rec f nodes l =
        match (nodes : INode list) with
        |[] -> l
        |x::xs -> f xs (h (x.Statm()) l)

    in (f (cfg.Nodes()) []) |> List.distinct 
;;

// map: aloc -> sizeOf(aloc)
let mutable SizeOf = new Dictionary<aloc,int>() ;;
let SizeOfAnalysis (cfg : ICFG) =
    let nodes = cfg.Nodes()
    let map = new Dictionary<aloc,int>()
    let rec f (ns : INode list) =
        match ns with
        |[] -> map
        |x::xs -> 
            match (x.Statm()) with 
            |GlobalDec(v) -> v |> List.map (fun x -> map.TryAdd(x,4) )     |> ignore ; f xs
            |HeapDec(v) ->   v |> List.map (fun (x,s) -> map.TryAdd(x,s) ) |> ignore ; f xs
            |_ -> f xs
    in f nodes
;;
    
(* Other todo analysis: #Malloc for counting #HeapRegs; #Procs for counting #ARRegs *)
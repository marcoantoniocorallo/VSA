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
let availableVarsAnalysis (cfg : ICFG<Exp>) : (aloc list) =

    // if statm = {HeapDec/GlobalDec} -> add new defined vars to ls
    let h statm ls = 
        match statm with 
        |GlobalDec(l) -> ls@l 
        |HeapDec(l) -> ls@(List.map fst l) 
        |Array(a,size,n) -> 
            let mutable newl = [] in 
            for i=0 to (size-1) do
                if i%4=0 then newl<-newl@[(a+"+"+(string i))] else ignore()
            ls@newl
        |_ -> ls

    let rec f nodes l =
        match (nodes : INode<Exp> list) with
        |[] -> l
        |x::xs -> f xs (h (x.Statm()) l)

    in (f (cfg.Nodes()) []) |> List.distinct 
;;

// map: aloc -> sizeOf(aloc)
let mutable SizeOf = new Dictionary<aloc,int>() ;;
let SizeOfAnalysis (cfg : ICFG<Exp>) =
    let nodes = cfg.Nodes()
    let map = new Dictionary<aloc,int>()
    let rec f (ns : INode<Exp> list) =
        match ns with
        |[] -> map
        |x::xs -> 
            match (x.Statm()) with 
            |GlobalDec(v) -> v |> List.map (fun x -> map.TryAdd(x,4) )     |> ignore ; f xs
            |HeapDec(v) ->   v |> List.map (fun (x,s) -> map.TryAdd(x,s) ) |> ignore ; f xs
            |Array(a,size,n) ->
                map.TryAdd(a,size) |> ignore
                for i=0 to (size-1) do
                    if i%4=0 then (map.TryAdd(a+"+"+(string i),4) |> ignore) else ignore()
                f xs
            |_ -> f xs
    in f nodes
;;

// A dummy estimate of #iterations to do before widening
let WideningDefaultValue = 15
let mutable WideningThreshold = WideningDefaultValue;;
let ThresholdCalc (cfg : ICFG<Exp>) =
    let nodes = cfg.Nodes()
    let rec f (ns : INode<Exp> list) l =
        match ns with
        |[] -> l
        |x::xs -> 
            match (x.Statm()) with
            |LeqConst(_,s) -> f xs l@[s]
            |GeqConst(_,s) -> f xs l@[s]
            |ArrayLeqConst(_,_,s) -> f xs l@[s]
            |ArrayGeqConst(_,_,s) -> f xs l@[s]
            |_ -> f xs l

    let g l = if l |> List.isEmpty then 0 else l |> List.max
    in g (f nodes [])+1
;;
    
(* Other todo analysis: #Malloc for counting #HeapRegs; #Procs for counting #ARRegs *)
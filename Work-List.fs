open System.Collections
open FSharp.Collections

(* Input : Lattice L
           Transfer functions t1,...,tn
           Control-Flow-Graph nodes v1,...,vn
   Output: Abstract states x1,...,xn
*)
let WorkList (L : ILattice<'T>) (T : Map<int,('T->'T)>) (cfg : ICFG)  =
    
    // define: n = #{v1,...,vn}, 
    // x=(<v1,x1>,...,<vn,xn>) abstract states, 
    // W=(v1,...,vn) cfg nodes in worker list
    let n = cfg.Length()
    let x = new Hashtable() 
    let mutable W = []
    let mutable v = cfg.EntryBlock()
    for i = 0 to n do
        x.Add(v, L.Bot)
        W <- W@[v]
        v <- v.Succ()

    // Scans work-list and applies transfer function based on type of statement
    let mutable i = 0
    while W.Length <> 0 do
        let vi = W.Head
        W <- W.Tail
        let y = x.[vi] |> T.[vi.statm]
        i <- i+1

        // for each vi's successor node, compute join and, if It's not alredy present, add It
        for vj in vi.Dep() do
            let z = L.Join x.[vj] y
            
            if x.[vj] <> z then 
                x.[vj]<-z
                W <- W@[vj]
    x
;;

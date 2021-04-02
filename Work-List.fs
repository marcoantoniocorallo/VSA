open System.Collections.Generic

(* Input : Lattice L
           Transfer functions t1,...,tk
           Control-Flow-Graph nodes v1,...,vn
   Output: Abstract states x1,...,xn
*)
    
let WorkList (L : ILattice<'T>) (T : Exp -> ('T->'T)) (cfg : ICFG)  =
    
    // define: n = #{v1,...,vn}, 
    // x = (node -> (aloc -> VS) ) = (⊥,...,⊥) 
    // W=(v1,...,vn) cfg nodes in worker list
    // Note: //Dict is mutable, as opposed to Map, and is generic, as opposed to htable
    let n = cfg.Length()
    let x = new Dictionary<INode,'T>()
    let mutable W = []
    for node in cfg.Nodes() do
        x.TryAdd(node, L.Bot()) |> ignore // ignore existing nodes (case of several branches that converges)
        W <- W@[node]

    // Scans work-list and applies transfer function based on type of statement
    while W.Length <> 0 do
        let vi = W.Head
        W <- W.Tail
        let y = x.[vi] |> T(vi.Statm())

        // for each vi's successor node, compute join and, if It's not already present, add It
        for vj in vi.Dep() do
            // let z = L.Join x.[vj] y // classic version
            let z = L.Widen x.[vj] y   // ensure termination
            if x.[vj] <> z then 
                x.[vj]<-z
                W <- W@[vj]
    x
;;
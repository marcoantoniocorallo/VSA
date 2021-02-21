type CFG( entryNode : Node) = 

    interface ICFG with

        member this.EntryBlock() = entryNode :> INode

        // number of nodes (including repetitions)
        member this.Length() = 
            let list = (entryNode :> INode).Succ()
            let rec f succ count = 
                match succ with
                |[] -> count
                |x::xs -> 
                        let rec g (succ1 : INode list) k = 
                            match succ1 with
                            |[] -> f xs k 
                            |y::ys -> g ys (k+(f (y.Succ()) 1))                        

                        in g (x.Succ()) count+1
            in f list 1
;;
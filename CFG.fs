type CFG( entryNode : Node) = 

    interface ICFG with

        member this.EntryBlock() = entryNode :> INode

        // returns the height of the graph, not the number of nodes
        member this.Length() = 
            let list = (entryNode :> INode).Succ()
            let rec f ((l : INode list), (count : int)) =
                match l with
                |[] -> count
                |x::xs -> f (x.Succ(), (count+1))
            in f (list, 1)

;;
type CFG( entryNode : Node) = 

    interface ICFG with

        member this.EntryBlock() = entryNode :> INode

        // number of (distinct) nodes 
        member this.Length() = (this:>ICFG).Nodes().Length

        // Returns list of all distinct nodes
        member this.Nodes() = [(this:>ICFG).EntryBlock()]@(this:>ICFG).EntryBlock().AllSucc([]) 
                              |> List.distinct 
                              |> List.sort

    end

;;

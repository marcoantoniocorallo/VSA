type CFG(entryNode : Node) = 

    interface ICFG<Exp> with

        member this.EntryBlock() = entryNode :> INode<Exp>

        // number of (distinct) nodes 
        member this.Length() = (this:>ICFG<Exp>).Nodes().Length

        // Returns list of all distinct nodes
        member this.Nodes() = [(this:>ICFG<Exp>).EntryBlock()]@(this:>ICFG<Exp>).EntryBlock().AllNodes([]) 
                              |> List.distinct 
                              |> List.sort

    end

;;

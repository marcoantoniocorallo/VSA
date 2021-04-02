type CFG( entryNode : Node) = 

    interface ICFG with

        member this.EntryBlock() = entryNode :> INode

        // number of (distinct) nodes 
        member this.Length() = (this:>ICFG).Nodes().Length

        // Returns list of all distinct nodes
        member this.Nodes() = 
            let list = (entryNode :> INode).Succ()

            let rec f succ l = 
                match succ with
                |[] -> l
                |x::xs -> 
                        let rec g (succ1 : INode list) l1 = 
                            match succ1 with
                            |[] -> f xs l1
                            |y::ys -> if List.contains y l1 then g ys l1 else g ys (l1@(f (y.Succ()) [y]))

                        in g (x.Succ()) (if List.contains x l then l else (l@[x]))

            in (f list [(entryNode:>INode)]) |> List.distinct |> List.sort

    end

;;

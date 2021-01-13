(* Represents a CFG node, with an expression and a Successor-nodes list, recursively *)

type Node( exp : Exp, succ : Node list ) = 

    interface INode with

        member this.Statm() = exp

        member this.Succ() = succ |> List.map (fun x -> x :> INode)

        // forward-analysis -> dep = succ
        member this.Dep() = (this :> INode).Succ()

;;
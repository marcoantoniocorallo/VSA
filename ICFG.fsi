[<Interface>]

type ICFG = 
    interface

    abstract EntryBlock : unit -> INode

    abstract Succ: INode -> INode

    abstract Prec: INode -> INode

    abstract Dep: INode -> INode list

    abstract Length: unit -> int

end
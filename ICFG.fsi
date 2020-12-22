[<Interface>]

type ICFG = 
    interface

    abstract EntryBlock : unit -> INode

    abstract Succ: INode -> INode list

    abstract Prec: INode -> INode list

    abstract Dep: INode -> INode list

    abstract Length: unit -> int

end
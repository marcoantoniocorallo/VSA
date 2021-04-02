[<Interface>]

type ICFG = 
    interface

    abstract EntryBlock : unit -> INode

    abstract Length: unit -> int

    abstract Nodes: unit -> INode list

end
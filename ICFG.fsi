[<Interface>]

type ICFG<'T> = 
    interface

    abstract EntryBlock : unit -> INode<'T>

    abstract Length: unit -> int

    abstract Nodes: unit -> INode<'T> list

end
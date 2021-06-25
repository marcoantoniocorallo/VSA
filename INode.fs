[<Interface>]

type INode<'T> =
    interface

    abstract ID : unit -> int

    abstract Statm : unit -> 'T

    abstract Succ: unit -> INode<'T> list

    abstract AllNodes : INode<'T> list -> INode<'T> list

    abstract Dep : unit -> INode<'T> list

    inherit System.IComparable 
        abstract CompareTo : obj -> int

end
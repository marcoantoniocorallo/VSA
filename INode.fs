[<Interface>]

type INode =
    interface

    abstract ID : unit -> int

    abstract Statm : unit -> Exp 

    abstract Succ: unit -> INode list

    abstract Dep : unit -> INode list

    inherit System.IComparable 
        abstract CompareTo : obj -> int

end
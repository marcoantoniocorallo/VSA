[<Interface>]

type INode =
    interface

    // statm : int is a stub
    // TODO : Statement type
    abstract statm : int

    abstract Succ: unit -> INode

    abstract Prec: unit -> INode

    abstract Dep : unit -> INode list

end
[<Interface>]

type INode =
    interface

    // statm : int is a stub
    // TODO : Statement type
    abstract statm : int

    abstract Succ: unit -> INode list

    abstract Prec: unit -> INode list

    abstract Dep : unit -> INode list

end
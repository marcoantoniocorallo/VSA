[<Interface>]

type INode =
    interface

    abstract Statm : unit -> Exp 

    abstract Succ: unit -> INode list

    abstract Dep : unit -> INode list

end
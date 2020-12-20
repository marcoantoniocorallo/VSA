[<Interface>]

type ILattice<'T> = 
    interface

    abstract Bot : unit -> 'T

    abstract Leq : 'T -> 'T -> bool

    abstract Join : 'T ->'T -> 'T

    abstract Meet : 'T -> 'T -> 'T

    abstract Top : unit -> 'T

    abstract Widen : 'T -> 'T -> 'T

    abstract Narrow : 'T -> 'T -> 'T

    abstract FiniteHeight : unit -> bool 

    abstract IsBotEmpty : unit -> bool

    abstract IsBot : 'T -> bool

    abstract IsTop : 'T -> bool

end
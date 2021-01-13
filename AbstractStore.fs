(* An abstract store for VSA is a map: a-loc -> Value-set *)
type AbstractStore( aloc : string, vs : ValueSet) =

    member this.Pair = (aloc, vs)

    member this.aLoc = aloc

    member this.VS = vs

;;
(* An abstract store for VSA is a map: a-loc -> Value-set 
 * ⊥ and ⊤ are symbolically represented by the couple <"0", VS(⊥)/VS(⊤)>, since 0 cannot be a variable name
 *)
type AbstractState( map : Map<aloc, ValueSet>) =

    member this.Map = map

    member this.ALocs = this.Map |> Map.toList |> List.map fst

    member this.VS = this.Map |> Map.toList |> List.map snd

    member this.IsBot() = 
        try
            match this.Map.["0"] with |vs -> if vs.IsBot() then true else false
        with | :? System.Collections.Generic.KeyNotFoundException -> false

    member this.IsTop() = 
        try
            match this.Map.["0"] with |vs -> if vs.IsTop() then true else false
        with | :? System.Collections.Generic.KeyNotFoundException -> false

    // build the map from list
    new(l : (aloc*ValueSet) list) = AbstractState(new Map<aloc,ValueSet>(l))

;;
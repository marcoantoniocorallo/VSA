(* An abstract store for VSA is a map: a-loc -> Value-set 
 * ⊥ = <aloc-i, VS.⊥> for each aloc-i
 * ⊤ = <aloc-i, VS.⊤> for each aloc-i
 *)
type AbstractState( map : Map<aloc, ValueSet>) =

    member this.Map = map

    member this.ALocs = this.Map |> Map.toList |> List.map fst

    member this.VS = this.Map |> Map.toList |> List.map snd

    member this.IsBot() = 
        let n = (this.VS |> List.filter (fun x -> x.IsBot()) ).Length
        if n = this.VS.Length then true else false

    member this.IsTop() = 
        let n = (this.VS |> List.filter (fun x -> x.IsTop()) ).Length
        if n = this.VS.Length then true else false

    // build the map from list
    new(l : (aloc*ValueSet) list) = AbstractState(new Map<aloc,ValueSet>(l))

;;
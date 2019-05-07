module Common

let (|?) = defaultArg

let map_or defval optval someFn  = 
    match optval with
    | Some v -> someFn v
    | None -> defval

// Computation expression for the Option monad.
type MaybeBuilder() =

    member this.Bind(x, f) = 
        match x with
        | None -> None
        | Some a -> f a

    member this.Return(x) = 
        Some x
    
    member this.Zero () = None
   
let maybe = new MaybeBuilder()
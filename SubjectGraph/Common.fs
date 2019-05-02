module Common

let (|?) = defaultArg

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
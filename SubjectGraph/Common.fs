module Common

let log = printfn

let (|?) = defaultArg

/// Where to put this? Either in common or NamePrefixIndex.
/// Return true if list1 is a strict prefix of list2 (not equal).
let rec isStrictPrefix list1 list2 = 
    match (list1, list2) with
        | (_, []) -> false
        | ([], y::ys) -> true
        | (x::xs, y::ys) -> x = y && isStrictPrefix xs ys

/// Apply a function to an option or use a default (from OCaml)
let mapOr someFn defval optval  = 
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
   
let maybe = MaybeBuilder()
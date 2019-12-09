module Common

let log = printfn

let (|?) = defaultArg

/// Append to a list if an option is Some.
let appendMaybe (l: System.Collections.Generic.List<_>) opt = 
    match opt with
    | Some v -> l.Add(v)
    | None -> ()

/// Where to put this? Either in common or NamePrefixIndex.
/// Return true if list1 is a strict prefix of list2 (not equal).
let rec isStrictPrefix list1 list2 = 
    match (list1, list2) with
        | (_, []) -> false
        | ([], y::ys) -> true
        | (x::xs, y::ys) -> x = y && isStrictPrefix xs ys

/// Apply a function to an option or use a default (from OCaml)
let mapOr someFn defval optval = 
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
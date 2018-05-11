module CallNumber

open System.Text.RegularExpressions
open System

exception BadCallNumberException of string

(* BEST REGEX EVER *)
(* Still to check: how many digits can cutter numbers and decimals have? *)
(* To fix: 1st cutter can be something besides letter + digits *)
(* groups: 1: letters, 2: number, 3:  decimal 4: rest1, 5: cutter1, 6: cutter2, 7: space+year *)
let LCCN_REGEX = @"^([A-Z]{1,3}) ?([0-9]{1,5})(\.[0-9]{1,4})? ?((\.[A-Z][0-9]{1,5}) ?([A-Z][0-9]{1,4})?)?( [0-9]{1,4}[a-z]?)?$"

let applyOption x someFn noThing = 
    match x with
        | Some v -> someFn v
        | None -> noThing

type LCCN = {
    (* Can I use CN's with no number to specify a whole category? 
     * How can I distinguish Z (whole category) from Z (General works)? 
     * Maybe need a separate type for category or range *)
    letters : string;
    number : int;
    decimal : Decimal option;
    cutter1 : (char * int) option;
    cutter2 : (char * int) option;
    date : string option; (* because of "2000b", etc. Lexicographic sorting should work? *)
} with
    member this.Show () = (* might like to try monadic stuff with this too *)
        this.letters + string this.number
        + applyOption this.decimal string ""
        + applyOption this.cutter1 (fun x -> "." + string (fst x) + string (snd x)) ""
        + applyOption this.cutter2 (fun x -> " " + string (fst x) + string (snd x)) ""
        + applyOption this.date string ""
    static member Parse cnstr = 
        let m = Regex.Match(cnstr, LCCN_REGEX)
        if m.Success then 
            let groups = [ for g in m.Groups -> g.Value ]
            printfn "%s" (groups.ToString ()) (* DEBUG *)
            { 
                letters = groups.[1];
                number = int (groups.[2]);
                decimal = if groups.[3] = "" then None 
                          else Some (decimal (groups.[3]));
                cutter1 = if groups.[5] = "" then None 
                          (* Skip initial dot *)
                          else Some (groups.[5].[1], int (groups.[5].[2..]));
                cutter2 = if groups.[6] = "" then None 
                          else Some (groups.[6].[0], int (groups.[6].[1..]));
                date = if groups.[7] = "" then None 
                          else Some (groups.[7].[1..]) (* Skip initial space *)
            }
        else raise (BadCallNumberException "Could not parse LOC Call Number")
    (* TODO: compare if one CN is a more specific version of another? *)
    // built-in compare seems to work fine so far.
    (* static member (<=) (cn1, cn2) = 
        printf "hi"
        cn1.letters <= cn2.letters 
        || cn1.number <= cn2.number *) 

type LCCNRange = {
    letters: string
} (* TODO: constructor from two CN's *)
  (* TODO: 'Contains' CN method *)

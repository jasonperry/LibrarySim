module CallNumber

open System.Text.RegularExpressions
open System

exception CallNumberError of string

(* BEST REGEX EVER *)
(* ISSUE: Some Gutenberg records have ONLY the letters. Is that a valid CN? *)
(* Still to check: how many digits can cutter numbers and decimals have? *)
(* To fix: 1st cutter can be something besides letter + digits *)
(* groups: 1: letters, 2: number, 3: decimal 4: rest1, 5: cutter1, 6: cutter2, 7: space+year *)
let LCCN_REGEX = @"^([A-Z]{1,3})"           // group 1: call letters
                 + " ?([0-9]{1,5})"         // g2: number
                 + "(\.[0-9]{1,4})? ?"      // g3: decimal
                 + "((\.[A-Z][0-9]{1,5})"   // g4: rest, g5: cutter1
                 + " ?([A-Z][0-9]{1,4})?)?" // g6: cutter2
                 + "( [0-9]{4}[a-z]?)?"     // g7: year
                 + "( [Vv]\.[0-9]+| [Cc]\.[0-9]+| [Pp]t\.[0-9]+| suppl.)?$" // g8: misc

// If a string doesn't parse as LCCN, see if it looks like just the letters part (gutenberg)
let isCNLetters s = 
    let isAlpha c = 'A' <= c && c <= 'z'
    String.forall isAlpha s && s.Length >= 1 && s.Length <= 3

let applyOption x someFn noThing = 
    match x with
        | Some v -> someFn v
        | None -> noThing

let (|?) = defaultArg

type LCCN = {
    // Can I use CN's with no number to specify a whole category? 
    // How can I distinguish Z (whole category) from Z (General works)? 
    letters : string;
    number : int;
    decimal : Decimal option;
    cutter1 : (char * int) option;
    cutter2 : (char * int) option;
    date : string option; // because of "2000b", etc. Lexicographic sorting should work? 
    misc : string option; // "v.1", "c.2", "suppl.", "Pt.1" 
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
                misc = if groups.[8] = "" then None
                       else Some (groups.[8].[1..]) 
            }
        else  // TODO: check if all letters. 
            //      ^-- but it's done by catching in the gutenberg code. Is that good?
            raise (CallNumberError "Could not parse LOC Call Number")
    (* TODO: compare if one CN is a more specific version of another? *)
    // built-in compare seems to work fine so far.
    (* static member (<=) (cn1, cn2) = 
        printf "hi"
        cn1.letters <= cn2.letters 
        || cn1.number <= cn2.number *) 

module CNRange = 

    type LCCNRange = {  // Should I rename it to T?
        startCN : LCCN
        endCN : LCCN
    } 
    
    let create startCN endCN = {startCN = startCN; endCN = endCN}
    let parse (s : string) = 
        let cnStrings = s.Split [|'-'|]
        if Array.length cnStrings = 2 then
            {startCN = LCCN.Parse cnStrings.[0]; 
             endCN = LCCN.Parse cnStrings.[1]}
        elif Array.length cnStrings = 1 then
            {startCN = LCCN.Parse cnStrings.[0]; 
             endCN = LCCN.Parse cnStrings.[0]}
        else 
            raise  (CallNumberError "Could not parse CN range")

    let contains range cn =       // range.contains cn
        range.startCN <= cn && cn <= range.endCN

(* TODO: constructor from two CN's *)
  (* TODO: 'Contains' CN method *)

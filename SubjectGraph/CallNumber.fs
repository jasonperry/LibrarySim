module CallNumber

open System.Text.RegularExpressions
open System

exception CallNumberError of string

// BEST REGEX EVER 
// ISSUE: Some Gutenberg records have ONLY the letters. Is that a valid CN? 
// Still to check: how many digits can cutter numbers and decimals have? 
// To fix: 1st cutter can be something besides letter + digits 
// groups: 1: letters, 2: number, 3: decimal 4: rest1, 5: cutter1, 6: cutter2, 7: space+year 
let LCCN_REGEX = @"^([A-Z]{1,3})"           // group 1: call letters
                 + " ?([0-9]{1,5})"         // g2: number
                 + "(\.[0-9]{1,4})? ?"      // g3: decimal
                 + "((\.[A-Z][0-9]{1,5})"   // g4: rest, g5: cutter1
                 + " ?([A-Z][0-9]{1,4})?)?" // g6: cutter2
                 + "( [0-9]{4}[a-z]?)?"     // g7: year
                 + "( [Vv]\.[0-9]+| [Cc]\.[0-9]+| [Pp]t\.[0-9]+| suppl.)?$" // g8: misc

/// Regex for prefixes of legal call numbers, used in the Class dataset
let LCCN_PARTIAL_REGEX = @"^([A-Z]{1,3})"           // group 1: call letters
                         + "( ?([0-9]{1,5})"        // g2: rest, g3: number
                         + "(\.[0-9]{1,4})? ?"      // g4: decimal
                         + "(\.[A-Z][0-9]{0,5})"   //  g5: cutter1 (maybe no number)
                         + " ?([A-Z][0-9]{0,4})?)?" // g6: cutter2 (maybe no number)

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
    letters : string;
    number : int;
    decimal : Decimal option;
    cutter1 : (char * int) option;
    cutter2 : (char * int) option;
    date : string option; // because of "2000b", etc. Lexicographic sorting should work? 
    misc : string option; // "v.1", "c.2", "suppl.", "Pt.1" 
} 
module LCCN = 
    let show (cn : LCCN) = (* might like to try monadic stuff with this too *)
        cn.letters + string cn.number
        + applyOption cn.decimal string ""
        + applyOption cn.cutter1 (fun x -> "." + string (fst x) + string (snd x)) ""
        + applyOption cn.cutter2 (fun x -> " " + string (fst x) + string (snd x)) ""
        + applyOption cn.date string ""
    let parse (cnString : string) = 
        let m = Regex.Match(cnString.ToUpper(), LCCN_REGEX)
        if m.Success then 
            let groups = [ for g in m.Groups -> g.Value ]
            // printfn "%s" (groups.ToString ()) (* DEBUG *)
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
        else
            raise <| CallNumberError ("Could not parse LOC Call Number " + cnString)
    /// True if two call numbers are the same except for year and misc.
    let sameTitle cn1 cn2 = 
        cn1.letters = cn2.letters && cn1.number = cn2.number
        && cn1.decimal = cn2.decimal && cn1.cutter1 = cn2.cutter1
        && cn1.cutter2 = cn2.cutter2
    /// True if first call number is more specific but otherwise equal. Equal is false.
    /// TODO: Should not be needed, in the ideal domain model.
    let moreSpecific cn1 cn2 = 
        cn1.letters = cn2.letters && cn1.number = cn2.number 
        && (cn1.decimal.IsSome && cn2.decimal.IsNone    // can short-circuit here
            || (cn1.decimal = cn2.decimal &&            // Okay if they're None?
                (cn1.cutter1.IsSome && cn2.cutter1.IsNone
                 || (cn1.cutter1 = cn1.cutter1 && 
                     (cn1.cutter2.IsSome && cn1.cutter2.IsNone)))))
    // built-in compare seems to work fine so far.
    (* static member (<=) (cn1, cn2) = 
        cn1.letters <= cn2.letters 
        || cn1.number <= cn2.number *) 

// TODO: Range *should* allow a 'letters-only' CN. Then how to compare? Make a "magic" numbers value?
type LCCNRange = { 
    startCN : LCCN
    endCN : LCCN
} 
module CNRange = // nice if it could be a functor over types of CNs...
    let create startCN endCN = 
        if startCN > endCN then
            raise (CallNumberError "Illegal CN Range: Start Call Number is higher")
        else 
            {startCN = startCN; endCN = endCN}
    let parse (s : string) = 
        let cnStrings = s.Split [|'-'|]
        if Array.length cnStrings = 2 then
            {startCN = LCCN.parse cnStrings.[0]; 
             endCN = LCCN.parse cnStrings.[1]}
        elif Array.length cnStrings = 1 then
            {startCN = LCCN.parse cnStrings.[0]; 
             endCN = LCCN.parse cnStrings.[0]}
        else 
            raise <| CallNumberError 
                     ("Wrong number of segments forCN range" + string(Array.length cnStrings))
    let contains range cn =       // range.contains cn
        range.startCN <= cn && cn <= range.endCN
    let isSubRange subrange range = 
        contains range subrange.startCN && contains range subrange.endCN


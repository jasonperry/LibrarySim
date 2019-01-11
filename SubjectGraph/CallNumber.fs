module CallNumber

open System
open System.Collections.Generic
open System.Text.RegularExpressions

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
    number : int option;
    decimal : Decimal option;
    cutter1 : (char * int option) option;
    cutter2 : (char * int option) option;
    date : string option; // because of "2000b", etc. Lexicographic sorting should work? 
    misc : string option; // "v.1", "c.2", "suppl.", "Pt.1" 
} 
module LCCN = 
    let show (cn : LCCN) = // might want to try monadic style with this too 
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
                number = Some (int (groups.[2]));
                decimal = if groups.[3] = "" then None 
                          else Some (decimal (groups.[3]));
                cutter1 = if groups.[5] = "" then None 
                          // Skip initial dot 
                          else Some (groups.[5].[1], Some (int (groups.[5].[2..])));
                cutter2 = if groups.[6] = "" then None 
                          else Some (groups.[6].[0], Some (int (groups.[6].[1..])));
                date = if groups.[7] = "" then None 
                       else Some (groups.[7].[1..]) (* Skip initial space *)
                misc = if groups.[8] = "" then None
                       else Some (groups.[8].[1..]) 
            }
        else
            let m = Regex.Match(cnString.ToUpper(), LCCN_PARTIAL_REGEX)
            if m.Success then
                let groups = [ for g in m.Groups -> g.Value ]
                {
                    letters = groups.[1];
                    number = if groups.[3] = "" then None
                             else Some (int (groups.[3]));
                    decimal = if groups.[4] = "" then None 
                              else Some (decimal (groups.[4]));
                    cutter1 = if groups.[5] = "" then None 
                              // Skip initial dot 
                              else Some (groups.[5].[1], 
                                         if groups.[5].Length > 2 
                                         then Some (int (groups.[5].[2..]))
                                         else None);
                    cutter2 = if groups.[6] = "" then None 
                              else Some (groups.[6].[0], 
                                         if groups.[6].Length > 1 
                                         then Some (int (groups.[6].[1..]))
                                         else None);
                    date = None;
                    misc = None
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
} // ToString?
module CNRange = // nice if it could be a functor over types of CNs...
    let create startCN endCN = 
        if startCN > endCN then
            raise (CallNumberError "Illegal CN Range: Start Call Number is higher")
        else 
            {startCN = startCN; endCN = endCN}
    // TODO: might want to put all this fancy code somewhere else.
    let patchCNSuffix (cnString: string) (suffixString: string) = 
        /// split CN string into segments by character class. Return two 
        /// lists, one with the char classes and one with the strings.
        let segmentCNString (cnString: string) = 
            let charClass c = 
                if Char.IsLetter c then 'l'
                elif Char.IsDigit c then 'd'
                elif Char.IsPunctuation c || Char.IsSeparator c then 'p'
                else raise <| 
                     CallNumberError ("Unrecognized character class for: " + string c)
            // let cnString = cnString.Replace(" ", "") // now treat space as punct.
            let charClasses = new List<char>()
            let segments = new List<string>()
            let mutable pos = 0
            while pos < cnString.Length do 
                let cClass = charClass cnString.[pos]
                charClasses.Add cClass
                let mutable segment = ""
                // move forward, adding characters until the char class changes
                while pos < cnString.Length && charClass cnString.[pos] = cClass do
                    segment <- segment + (string cnString.[pos])
                    pos <- pos + 1
                segments.Add segment
            (List.ofSeq segments, List.ofSeq charClasses)
        /// Return the first position in the left list at which the right list 
        /// matches the entire remainder. 
        let leftSuffixMatch (left: 'a list) (right: 'a list) = 
            let mutable pos = 0
            let mutable found = false
            while pos < left.Length && not found do 
                let mutable lpos = pos
                let mutable rpos = 0
                while lpos < left.Length && rpos < right.Length && left.[lpos] = right.[rpos] do
                    lpos <- lpos + 1
                    rpos <- rpos + 1
                // if it got to the end of left, we're good
                if lpos = left.Length then
                    found <- true
                else
                    pos <- pos + 1
            if found then pos else -1
        let sameClass (c1: char) (c2: char) = 
            Char.IsDigit c1 && Char.IsDigit c2 ||
            Char.IsLetter c1 && Char.IsLetter c2 ||
            Char.IsPunctuation c1 && Char.IsPunctuation c2
        // big try: 
        let cnSegments, classes = segmentCNString cnString
        let _, suffixClasses = segmentCNString suffixString
        let matchPos = leftSuffixMatch classes suffixClasses
        if matchPos = -1 then
            raise <| CallNumberError 
                     ("Could not match CN suffix " + cnString + ", " + suffixString)
        else 
            (String.concat "" cnSegments.[..matchPos-1]) + suffixString

        (* try 1 // scan backwards until there's a match at a position where the character
        // class changes (which should be a boundary between CN segments)
        let mutable pos = cnString.Length - 1
        while (cnString.[pos] <> suffixString.[0] || 
               sameClass cnString.[pos - 1] cnString.[pos]) do
            pos <- pos - 1
        cnString.[..pos] + suffixString *)
        (* // try 2: scan backwards until char class matches, then stop when it doesn't
        let mutable pos = cnString.Length - 1
        while not (sameClass cnString.[pos] suffixString.[0]) do 
            pos <- pos - 1
        while sameClass cnString.[pos] suffixString.[0] do 
            pos <- pos - 1 
        cnString.[..pos] + suffixString *)
    let parse (s : string) = 
        let cnStrings = s.Split [|'-'|]
        if Array.length cnStrings = 2 then
            {startCN = LCCN.parse cnStrings.[0]; 
             endCN = 
                try
                    LCCN.parse cnStrings.[1]
                with CallNumberError _ -> 
                    // we just let this throw if it fails.
                    try
                        let patched = patchCNSuffix cnStrings.[0] cnStrings.[1]
                        printfn "Successfully patched %s with suffix %s: %s" 
                                cnStrings.[0] cnStrings.[1] patched // DEBUG
                        LCCN.parse patched
                    with CallNumberError msg ->
                        printfn "Failed patch or parse: %s" msg
                        LCCN.parse cnStrings.[0] // MAY CHANGE - just to get a result
            }
        elif Array.length cnStrings = 1 then
            {startCN = LCCN.parse cnStrings.[0]; 
             endCN = LCCN.parse cnStrings.[0]}
        else 
            raise <| CallNumberError 
                     (sprintf "Wrong number of segments (%d) for CN range: %A" 
                              (Array.length cnStrings) cnStrings)
    let contains range cn =       // range.contains cn
        range.startCN <= cn && cn <= range.endCN
    let isSubRange subrange range = 
        contains range subrange.startCN && contains range subrange.endCN


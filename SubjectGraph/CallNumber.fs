module CallNumber

open System
open System.Collections.Generic
open System.Text.RegularExpressions

open Common

exception CallNumberError of string

// BEST REGEX EVER 
// Still to check: how many digits can cutter numbers and decimals have? 
// To fix: 1st cutter can be something besides letter + digits 
let LCCN_REGEX = @"^([A-Z]{1,3})"           // group 1: call letters
                 + " ?([0-9]{1,5})"         // g2: number // Removed optional space. Not in Class dataset.
                 + "(\.[0-9]{1,5})? ?"      // g3: decimal
                 + "((\.[A-Z](?:[0-9]{1,6})?)"   // g4: rest, g5: cutter1, 
                 + "(?:[\. ]?([A-Z]([0-9]{1,6})?))?)?" // g6: cutter2, g7: cutter2 number // formerly one
                 + "( [0-9]{4}[a-z]?)?"     // g8: date
                 + "( [Vv]\. ?[0-9]+| [Cc]\. ?[0-9]+| [Pp]t\. ?[0-9]+| suppl.)?$" // g9: misc

/// Regex for prefixes of legal call numbers, used in the Class dataset
let LCCN_PARTIAL_REGEX = @"^([A-Z]{1,3})"           // group 1: call letters
                         + "( ?([0-9]{1,5})"        // g2: rest, g3: number
                         + "(\.[0-9]{1,4})? ?"      // g4: decimal
                         + "(\.[A-Z][0-9]{0,5})"   //  g5: cutter1 (maybe no number)
                         + "([\. ][A-Z][0-9]{0,4})?)?" // g6: cutter2 (maybe no number)

/// The LCCN Record type. TODO: Call number interface for other types.
type LCCN = {
    letters : string;
    number : int option; // optional for partial CNs; should be in all complete CNs.
    decimal : Decimal option;
    cutter1 : (char * Decimal option) option; 
    cutter2 : (char * int option) option;
    date : string option; // because of "2000b", etc. Lexicographic sorting should work? 
    misc : string option; // "v.1", "c.2", "suppl.", "Pt.1" 
} 
module LCCN = 
    /// Chop off one field of the CN, for a more general match.
    let shortenByOneField (cn : LCCN) = 
        match cn.misc with
        | Some _ -> {cn with misc = None}
        | None -> 
          match cn.date with 
          | Some _ -> {cn with date = None}
          | None -> 
            match cn.cutter2 with 
            | Some _ -> {cn with cutter2 = None}
            | None -> 
              match cn.cutter1 with
              | Some _ -> {cn with cutter1 = None}
              | None -> 
                match cn.decimal with
                | Some _ -> {cn with decimal = None}
                | None -> 
                  match cn.number with
                  | Some _ -> {cn with number = None}
                  | None -> cn

    /// If a string doesn't parse as LCCN, see if it looks like just the letters part (gutenberg)
    let isCNLetters s = 
        let isAlpha c = 'A' <= c && c <= 'z'
        String.forall isAlpha s && s.Length >= 1 && s.Length <= 3

    let lettersOnlyCN letterStr = { letters = letterStr; 
                                    number = None;
                                    decimal = None;
                                    cutter1 = None;
                                    cutter2 = None;
                                    date = None;
                                    misc = None }
    let isLettersOnly (cn : LCCN) = 
        cn.number = None

    let toString (cn : LCCN) = // might want to try monadic style with this too 
        cn.letters + map_or "" cn.number string
        + map_or "" cn.decimal string
        + map_or "" cn.cutter1 (fun x -> 
            "." + string (fst x) + map_or "" (snd x) string)
        + map_or "" cn.cutter2 (fun x -> 
            " " + string (fst x) + map_or "" (snd x) string)
        + map_or "" cn.date string

    let parse (cnString : string) = 
        let m = Regex.Match(cnString, LCCN_REGEX)  // Removed ToUpper; fixes should be before.
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
                          else Some (groups.[5].[1], 
                                     if groups.[5].Length > 2
                                     then Some (Decimal.Parse ("." + (groups.[5].[2..])))
                                     else None)
                          ;
                cutter2 = if groups.[6] = "" then None 
                          else Some (groups.[6].[0], 
                                     if groups.[7] = "" then None
                                     else Some (int (groups.[7])));
                date = if groups.[8] = "" then None 
                       else Some (groups.[8].[1..]) (* Skip initial space *)
                misc = if groups.[9] = "" then None
                       else Some (groups.[9].[1..]) 
            }
        else
            raise <| CallNumberError ("Could not parse LOC Call Number " + cnString)
            (* let m = Regex.Match(cnString, LCCN_PARTIAL_REGEX) // removed toUpper()
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
                                         then Some (Decimal.Parse ("." + (groups.[5].[2..])))
                                         else None);
                    cutter2 = if groups.[6] = "" then None 
                              else Some (groups.[6].[0], 
                                         if groups.[6].Length > 2 
                                         then Some (int (groups.[6].[2..]))
                                         else None);
                    date = None;
                    misc = None
                }
            else
                raise <| CallNumberError ("Could not parse LOC Call Number " + cnString) *)
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

    let toString range = 
        (LCCN.toString range.startCN) + "-" + (LCCN.toString range.endCN)

    // TODO: might want to put all this fancy code somewhere else.
    /// Attempt to splice what might be the tail of a call number onto an existing one.
    let patchCNSuffix cnString suffixString = 
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
            let mutable startPos = 0
            let mutable found = false
            while startPos < left.Length && not found do 
                let mutable lpos = startPos
                let mutable rpos = 0
                while lpos < left.Length && rpos < right.Length && left.[lpos] = right.[rpos] do
                    lpos <- lpos + 1
                    rpos <- rpos + 1
                // if it got to the end of left, we're good
                if lpos = left.Length then
                    found <- true
                else
                    startPos <- startPos + 1
            if found then startPos else -1
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
        //printfn "Trying to parse CN %s" s
        let cnStrings = s.Split [|'-'|]
        if Array.length cnStrings = 1 then
            // Same for start and end. If it won't parse, there's nothing we can do.
            {startCN = LCCN.parse cnStrings.[0]; 
            endCN = LCCN.parse cnStrings.[0]}
        else
            let (startCNStr, endCNStr) = 
                if Array.length cnStrings = 2 then (cnStrings.[0], cnStrings.[1])
                // The funny case in the K's.
                elif Array.length cnStrings = 4 && cnStrings.[0] = cnStrings.[2] then
                    (cnStrings.[1], cnStrings.[3])
                // TODO: 3-parters, like [|"CR5744.A2"; "CR5744.A3"; "Z"|]
                else 
                    raise <| CallNumberError 
                     (sprintf "Unrecognized segment format for CN range: %A" cnStrings)
            let startCNParsed  = LCCN.parse startCNStr; // if it throws, it throws.
            let endCNParsed = 
                if String.IsNullOrEmpty(endCNStr) then 
                    startCNParsed 
                // If doesn't start with same letter as first, also patch.
                elif startCNStr.[0] <> endCNStr.[0] then
                    // TODO: pull this out as a function
                    try
                        printfn "Initial letters don't match (%s,%s), trying patch: " startCNStr endCNStr
                        let endStrPatched = patchCNSuffix cnStrings.[0] cnStrings.[1]
                        printfn "    Successfully patched %s with suffix %s: %s" 
                                cnStrings.[0] cnStrings.[1] endStrPatched // DEBUG
                        LCCN.parse endStrPatched
                    with CallNumberError msg ->
                        printfn "    Failed patch or parse: %s" msg
                        startCNParsed // What else could we do here?
                else
                    try
                        LCCN.parse endCNStr
                    with CallNumberError _ -> 
                        try
                            printfn "End call number (%s) didn't parse, trying patch" endCNStr
                            let endStrPatched = patchCNSuffix cnStrings.[0] cnStrings.[1]
                            printfn "    Successfully patched %s with suffix %s: %s" 
                                    cnStrings.[0] cnStrings.[1] endStrPatched // DEBUG
                            LCCN.parse endStrPatched
                        with CallNumberError msg ->
                            printfn "    Failed patch or parse: %s" msg
                            startCNParsed // should probably change
            if endCNParsed >= startCNParsed
            then {startCN = startCNParsed; endCN = endCNParsed}
            else 
                printfn "Warning: Start of CN range %s isn't <= %s: swapping" startCNStr endCNStr
                {startCN = endCNParsed; endCN = startCNParsed}
            
    let contains range cn =       // range.contains cn
        range.startCN <= cn && cn <= range.endCN
            // Should I check "moreSpecific" if start and end are the same?
            // This catches the case where e.g. the last B "BX" is meant to include "BX7864"
            || LCCN.isLettersOnly range.endCN && cn.letters = range.endCN.letters

    let isSubRange subrange range = 
        contains range subrange.startCN && contains range subrange.endCN

/// Tree for parent/child relationships of CN Ranges.
/// Lookup of existing nodes can be done in SubjectGraph.CNIndex
/// TODO: Needs parent?
/// Should be obsoleted by improved node insertion code.
(* 
type CNRangeTree = { range: LCCNRange; children: CNRangeTree list }
with 
    /// Insert returns the node above where it was inserted, or the node itself
    /// it it's a new root.
    member this.insert cnRange = 
        // special case: insert above top node. Hope it won't happen?
        if CNRange.isSubRange this.range cnRange
        then // new root.
            { range = cnRange; children = [this] }
        else 
            match List.tryFind (fun nd -> CNRange.isSubRange cnRange nd.range) this.children with
            // wait. It could either be a sibling or parent of all or some of the children!!
            // What if it's a parent of some? Should replace just those and be a sibling of the rest.
            // Wait again. This code should be where a node is inserted in the graph. 
            //   ** Is what we need just a variable isChild function to pass in?? **
            | None -> 
                let myChildren = List.filter 
                { range = this.range; 
                        children = ({range=cnRange; children=[]})::this.children }
            | Some subtree -> subtree.insert cnRange
    member this.findParent cnRange = 
        if CNRange.isSubRange this.range cnRange
        then    
            { range = cnRange; children = [this] }
        else
*)
            
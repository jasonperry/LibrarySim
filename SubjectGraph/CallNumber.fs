module CallNumber

open System
open System.Collections.Generic
open System.Text.RegularExpressions

open Common

exception CallNumberException of string

// BEST REGEX EVAR 
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
    cutter2 : (char * Decimal option) option;
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
        cn.letters + mapOr string "" cn.number 
        + mapOr (fun d -> (string d).[1..]) "" cn.decimal  // no leading zero
        + mapOr (fun x -> 
                    "." + string (fst x) + mapOr (fun d -> (string d).[2..]) "" (snd x) )
                "" cn.cutter1 // no leading zero or dot
        + mapOr (fun x -> 
                    " " + string (fst x) + mapOr (fun d -> (string d).[2..]) "" (snd x) ) // no zero or dot
                "" cn.cutter2
        + mapOr (fun d -> " " + string d) "" cn.date

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
                                     else Some (Decimal.Parse ("." + groups.[7])));
                date = if groups.[8] = "" then None 
                       else Some (groups.[8].[1..]) (* Skip initial space *)
                misc = if groups.[9] = "" then None
                       else Some (groups.[9].[1..]) 
            }
        else
            // DELETED: code to try partial match.
            raise <| CallNumberException ("Could not parse LOC Call Number " + cnString)

    /// True if two call numbers are the same except for year and misc.
    let sameTitle cn1 cn2 = 
        cn1.letters = cn2.letters && cn1.number = cn2.number
        && cn1.decimal = cn2.decimal && cn1.cutter1 = cn2.cutter1
        && cn1.cutter2 = cn2.cutter2
    /// True if first call number is more specific but otherwise equal. Equal is false.
    /// Needed for correct range containment, using only <= misses edge cases.
    let isMoreSpecific cn1 cn2 = 
        cn1.letters = cn2.letters
        && (cn1.number.IsSome && cn2.number.IsNone // any cases where number is none & rest isn't?
            // new simplified version.
            // || cn1.number = cn2.number && cn1.decimal = cn2.decimal
            || cn1.number = cn2.number 
            && (cn1.decimal.IsSome && cn2.decimal.IsNone && cn2.cutter1.IsNone
                || cn1.decimal = cn2.decimal  
                && (cn1.cutter1.IsSome && cn2.cutter1.IsNone && cn2.cutter2.IsNone
                    || cn1.cutter1.IsSome && cn2.cutter1.IsSome 
                    && (fst (cn1.cutter1.Value) = fst (cn2.cutter1.Value)
                    && (snd (cn1.cutter1.Value)).IsSome && (snd (cn2.cutter1.Value)).IsNone
                    && cn2.cutter2.IsNone // Probably can't have a 2nd cutter without the first?
                    || cn1.cutter1.Value = cn2.cutter1.Value // whole thing
                    && (cn1.cutter2.IsSome && cn2.cutter2.IsNone
                        || cn1.cutter2.IsSome && cn2.cutter2.IsSome 
                        && fst (cn1.cutter2.Value) = fst (cn2.cutter2.Value)
                        && (snd (cn1.cutter2.Value)).IsSome && (snd(cn2.cutter2.Value)).IsNone)))))
    
    /// Return which of two call numbers is more specific, or the first if they 
    /// are equally specific. 
    let mostSpecificCN cn1 cn2 = 
        if cn1.number.IsSome && cn2.number.IsNone then cn1
        elif cn1.number.IsNone && cn2.number.IsSome then cn2
        elif cn1.number.IsSome && cn2.number.IsSome then
            if cn1.decimal.IsSome && cn2.decimal.IsNone then cn1
            elif cn1.decimal.IsNone && cn2.decimal.IsSome then cn2
            elif cn1.decimal.IsSome && cn2.decimal.IsSome then 
                if cn1.cutter1.IsSome && cn2.cutter1.IsNone then cn1
                elif cn1.cutter1.IsNone && cn2.cutter1.IsSome then cn2
                elif cn1.cutter1.IsSome && cn2.cutter2.IsSome then
                    if cn1.cutter2.IsSome && cn2.cutter2.IsNone then cn1
                    elif cn1.cutter2.IsNone && cn2.cutter2.IsSome then cn2
                    else cn1
                else cn1
            else cn1
        else cn1
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
            raise (CallNumberException "Illegal CN Range: Start Call Number is higher")
        else 
            {startCN = startCN; endCN = endCN}

    let toString range = 
        (LCCN.toString range.startCN) 
        + (if range.endCN = range.startCN 
           then "" else "-" + (LCCN.toString range.endCN))

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
                     CallNumberException ("Unrecognized character class for: " + string c)
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
            raise <| CallNumberException 
                     ("Could not match CN suffix " + cnString + ", " + suffixString)
        else 
            (String.concat "" cnSegments.[..matchPos-1]) + suffixString

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
                    raise <| CallNumberException 
                     (sprintf "Unrecognized segment format for CN range: %A" cnStrings)
            let startCNParsed  = LCCN.parse startCNStr; // if it throws, it throws.
            let endCNParsed = 
                if String.IsNullOrEmpty(endCNStr) then 
                    startCNParsed 
                // If doesn't start with same letter as first, also patch.
                elif startCNStr.[0] <> endCNStr.[0] then
                    // TODO: pull this out as a function
                    try
                        Logger.Info "Initial letters don't match (%s,%s), trying patch: " startCNStr endCNStr
                        let endStrPatched = patchCNSuffix cnStrings.[0] cnStrings.[1]
                        Logger.Info "    Successfully patched %s with suffix %s: %s" 
                            cnStrings.[0] cnStrings.[1] endStrPatched
                        LCCN.parse endStrPatched
                    with CallNumberException msg ->
                        Logger.Warning "    Failed patch or parse: %s" msg
                        startCNParsed // What else could we do here?
                else
                    try
                        LCCN.parse endCNStr
                    with CallNumberException _ -> 
                        try
                            Logger.Info "End call number (%s) didn't parse, trying patch" endCNStr
                            let endStrPatched = patchCNSuffix cnStrings.[0] cnStrings.[1]
                            Logger.Info "    Successfully patched %s with suffix %s: %s" 
                                    cnStrings.[0] cnStrings.[1] endStrPatched // DEBUG
                            LCCN.parse endStrPatched
                        with CallNumberException msg ->
                            Logger.Warning "    Failed patch or parse: %s" msg
                            startCNParsed // should probably change
            if endCNParsed >= startCNParsed
            then {startCN = startCNParsed; endCN = endCNParsed}
            else 
                Logger.Warning "Start of CN range %s isn't <= %s: swapping" startCNStr endCNStr
                {startCN = endCNParsed; endCN = startCNParsed}
            
    let contains range cn =
        range.startCN <= cn && cn <= range.endCN
        // This catches the case where e.g. the last B "BX" is meant to include "BX7864"
        || range.startCN <= cn && LCCN.isMoreSpecific cn range.endCN

    let isSubRange subrange range = 
        // Update to not use contains, but its own logic.
        range.startCN <= subrange.startCN && 
            (subrange.endCN <= range.endCN 
             // heuristic: only allow the moreSpecific case if 
             // 1) it's not a single-CN range,  2) they don't have the same start.
             || range.startCN <> range.endCN && range.startCN <> subrange.startCN
                && LCCN.isMoreSpecific subrange.endCN range.endCN)

    /// Comparison function used to sort output.
    let compare ropt1 ropt2 = 
        match ropt1 with 
        | None -> if Option.isNone ropt2 then 0 else -1
        | Some range1 -> 
            match ropt2 with
            | None -> 1
            | Some range2 -> 
                if range1.startCN < range2.startCN then -1
                elif range1.startCN = range2.startCN then 0
                else 1

    let mostSpecificRange r1 r2 = 
        // simplification: just look at the starts.
        if LCCN.mostSpecificCN r1.startCN r2.startCN = r1.startCN then
            r1
        else
            r2
        (* if r1.startCN = r1.endCN && r2.startCN <> r2.endCN then r1
        elif r1.startCN <> r1.endCN && r2.startCN = r2.endCN then r2
        elif r1.startCN = r1.endCN && r2.startCN = r2.endCN then
            if LCCN.mostSpecificCN r1.startCN r2.startCN = r2.startCN then r2 else r1
        else // they're both ranges, so we don't care
            r1 *)


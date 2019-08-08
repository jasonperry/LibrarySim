/// Build a graph out of the LOC Class MarcXML dataset.
module BuildLCClassGraph

(* #I __SOURCE_DIRECTORY__
//#I @"C:\Users\Jason\code\LibrarySim\packages"
#r @"C:\Users\Jason\.nuget/packages/FSharp.Data/3.0.0/lib/net45/FSharp.Data.dll" // version shouldn't matter.
// #r @"C:\Users\Jason\code\LibrarySim\packages\FSharp.Core\4.5.2\lib\netstandard1.6\FSharp.Core.dll"
// #r @"C:\Users\Jason\code\LibrarySim\packages\FSharp.Data\3.0.0\lib\netstandard2.0\FSharp.Data.dll"
//#r ".nuget/packages/FSharp.Data/3.0.0/lib/netstandard2.0/FSharp.Data.dll"
#r "System.Xml.Linq.dll" // guess it's the net45 version. How could I replace it?
// #r @"C:\Users\Jason\code\LibrarySim\packages\binaryformatter\2.1.4\lib\netstandard1.1\BinaryFormatter.dll"
// #r @"C:\Users\Jason\.nuget\packages\system.xml.xdocument\4.3.0\lib\netcore50\System.Xml.XDocument.dll"
#r "obj/Debug/net461/SubjectGraph.exe"
//#r "obj/Debug/netcoreapp2.1/SubjectGraph.dll" *)

open System.Collections.Generic // Dictionary
open System.IO // for file read and write 
open System.IO.Compression
open System.Xml
open FSharp.Data

open Common
open MarcXml
open BookTypes
open CallNumber
open SubjectGraph
// open BuildTopLevel // It's passed in now.

let mutable nodeCount = 0

let npIndex = NamePrefixIndex.Create()

/// Parse all subfields of "See also" and create a CrossRefInfo object. 
/// TODO: function like this for 153 also (there can be multiple call numbers)
let parse253 (datafield: MarcXmlType.Datafield) = 
    let mutable desc =
        if datafield.Subfields.[0].Code = "i" then
            datafield.Subfields.[0].Value
        else ""
    let mutable i = if desc = "" then 0 else 1
    let rec scanSubfields () =
        if i >= datafield.Subfields.Length then
            []
        else
            let sfi = datafield.Subfields.[i]
            match sfi.Code with
            | "a" -> 
                let cnStartStr = 
                    if sfi.Value.EndsWith(";") || sfi.Value.EndsWith(",") || sfi.Value.EndsWith("+")
                    then sfi.Value.[0..(sfi.Value.Length - 2)]
                    else sfi.Value
                let cnEndStr = 
                    if datafield.Subfields.Length > i+1 && datafield.Subfields.[i+1].Code = "c" then
                        i <- i+1 
                        if datafield.Subfields.[i].Value.EndsWith(";") 
                           || datafield.Subfields.[i].Value.EndsWith(",") 
                           || datafield.Subfields.[i].Value.EndsWith("+")
                        then datafield.Subfields.[i].Value.[0..(datafield.Subfields.[i].Value.Length - 2)]
                        else datafield.Subfields.[i].Value
                    else ""
                let desc = 
                    if datafield.Subfields.Length > i+1 && datafield.Subfields.[i+1].Code = "i" then
                        i <- i + 1
                        datafield.Subfields.[i].Value
                    else ""
                i <- i + 1
                // parsing exceptions shoud be caught by caller.
                let parsedStart = LCCN.parse cnStartStr
                ({startCN = parsedStart; 
                    endCN = if cnEndStr = "" then parsedStart else LCCN.parse cnEndStr}, 
                 desc, None) :: scanSubfields ()
            | _ -> // FIXME: should detect an "i" or "z" here and do the right thing.
                printfn "Expected 253 subfield code 'a', got '%s'" sfi.Code
                desc <- desc + ". " + sfi.Value
                i <- i + 1
                scanSubfields ()
    { desc = desc; refs = scanSubfields () } // Caller should log error if empty.

/// Add node to graph by calling SubjectGraph.insertNode with the 
/// CNRange comparison function. 
let insertNode (graph: SubjectGraph) node = 
    match npIndex.FindExact(node.subdividedName) with 
        | Some exactMatch -> 
            // Should this be done in the SubjectGraph insert code?
            printfn "Subject %A already in index" node.name 
            exactMatch
        | None -> 
            match node.callNumRange with
            | Some cn -> 
                let isNarrower = fun n1 n2 -> CNRange.isSubRange n1.callNumRange.Value n2.callNumRange.Value
                SubjectGraph.insertNode isNarrower graph node 
            | None -> 
                printfn "No call number for %s, not inserting (for now)" node.name
            node

/// Create a whole sequence of SubjectNode objects and insert them with 
/// insertNode.
let addClassRecords theGraph (records : MarcXmlType.Record seq) = 
    // let theGraph = SubjectGraph.emptyGraph()
    let mutable recordCount = 0
    let mutable withNoCallNum = 0
    let mutable callNumCount = 0
    let mutable crossRefCount = 0
    let mutable recordsAdded = 0
    for record in records do
        (*let recEnum = records.GetEnumerator()
        while recEnum.MoveNext() && recordCount < 20000 do 
        let record = recEnum.Current *)
        let mutable controlNumber = null
        let mutable cnRangeStr = None
        let subjectNames = new List<string>()
        let mutable crossRefs = None
        callNumCount <- 0
        for controlfield in record.Controlfields do
            if controlfield.Tag = "001" then
                controlNumber <- controlfield.Value
        for datafield in record.Datafields do
            // Is it always okay to get the control number from the datafield
            //   instead of control field?
            (* if datafield.Tag = "10" then
                controlNumber <- getSingleSubfield datafield "a"
                // remove the space.
                match controlNumber with
                | Some s -> controlNumber <- Some (s.Replace (" ", ""))
                | None -> ()
            *)
            if datafield.Tag = "153" then
                // DELETED: former attempt to use computation expressions
                // Note: Some have alt call numbers at "c". How to deal? or is it a coding error?
                let tableField = getSingleSubfield datafield "z" |? ""
                let lcCallNumStart = // Option.lift2 (+) tableField (getSingleSubfield datafield "a")
                    match getSingleSubfield datafield "a" with 
                    | None -> None
                    | Some _ when tableField.Contains("-") -> None
                    | Some a -> Some (tableField + a)
                let lcCallNumEnd = 
                    match getSingleSubfield datafield "c" with 
                    | None -> None
                    | Some _ when tableField.Contains("-") -> None
                    | Some c -> Some (tableField + c)
                cnRangeStr <- 
                    match lcCallNumStart with 
                        | Some startStr -> 
                            match lcCallNumEnd with
                                | Some endStr -> Some (startStr + "-" + endStr)
                                | None -> Some startStr
                        | None -> None 
                subjectNames.AddRange(getAllSubfields datafield "h")
                subjectNames.AddRange(getAllSubfields datafield "j")
                callNumCount <- callNumCount + 1

            if datafield.Tag = "253" then // "See" cross reference
                let seeAlso = 
                    try
                        Some (parse253 datafield)
                    with CallNumberError errstr -> 
                        printfn "CallNumberError in field 253: %s" errstr
                        None
                // DEBUG: May remove this check if it never happens.
                if Option.isSome seeAlso && seeAlso.Value.desc = "" && seeAlso.Value.refs.IsEmpty then
                    printfn "Warning: empty 'see' data for %A; not adding" controlNumber
                else 
                    if Option.isSome cnRangeStr then
                        printfn "[INFO]: Got 'see also' info for %s" cnRangeStr.Value
                        crossRefCount <- crossRefCount + 1
                    crossRefs <- seeAlso

        if callNumCount = 0 || cnRangeStr.IsNone then
            withNoCallNum <- withNoCallNum + 1
            Logger.Error <| "No call number entry (153) or string for record " + controlNumber
        elif subjectNames.Count > 0 && 
            (subjectNames.[0].StartsWith("Table for") 
             || subjectNames.[0].StartsWith("Table of")
             || subjectNames.[0].StartsWith("Tables of")
             || subjectNames.[0].StartsWith("Table 1 of")
             || subjectNames.[0].StartsWith("Table 2 of")
             || subjectNames.[0].StartsWith("Table 3 of")
             || subjectNames.[0].StartsWith("Societies table")) then
            // TODO: just try to parse the CN here, and skip if it fails.
            Logger.Info <| "Skipping table entry " + controlNumber
        elif subjectNames.Count > 0 && subjectNames.[0].StartsWith("Learned societies (1") then
            Logger.Info <| "Skipping 'Learned societies' table for entry " + controlNumber
        else 
            recordsAdded <- recordsAdded + 1
            insertNode theGraph {
                uri = System.Uri ("http://knowledgeincoding.net/cnsubject/" + controlNumber);
                name = SubjectNode.joinSubjectName (List.ofSeq subjectNames); 
                subdividedName = List.ofSeq subjectNames;
                cnString = cnRangeStr; 
                callNumRange = 
                    match cnRangeStr with
                    | Some rstr -> 
                        try 
                            Some (CNRange.parse rstr)
                        with
                        | CallNumberError errstr -> 
                            printfn "Error with CNRange #%d: %s" recordCount errstr 
                            None
                    | None -> None
                    ;
                broader = new List<SubjectNode>(); 
                narrower = new List<SubjectNode>();
                seeAlso = crossRefs;
                books = new List<BookRecord>();
                booksUnder = 0
            } |> ignore
            // tried this way, not what we want...
            (* SubjectGraph.addSubject theGraph (SubjectNode.joinSubjectName (List.ofSeq subjectNames)) cnRangeStr
            |> ignore *)
        recordCount <- recordCount + 1
        if recordCount % 1111 = 0 then 
            printfn "============= %d\n" recordCount
            // printfn "============= %d\n%A\n" recordCount subjectNames
    printfn "Processed %d records"  recordCount 
    printfn "          %d with no call numbers" withNoCallNum
    printfn "          %d with cross-refs and CNs" crossRefCount
    printfn "          %d added to graph" recordsAdded
    theGraph

/// Build and save the LC Call Number Class Graph.
let buildGraph filename outputGraphFileName = 
    let reader = getXmlReader filename
    try 
        let startGraph = BuildTopLevel.buildGraph () // OR SubjectGraph.emptyGraph()
        let theGraph = addClassRecords startGraph (getRecordSeq reader)
        // SubjectGraph.makeTopLevel theGraph // mutates; guess it should be OO.
        // printfn "** Nodes in Top Level: %d" theGraph.topLevel.Count
        reader.Close()
        printfn "Updating Cross-references..."
        SubjectGraph.updateCrossrefs theGraph
        // instream.Close()
        // desperate attempt to reclaim memory before serialization.
        // ...now not needed? I found a better pickler.
        // npIndex.Clear()
        // System.GC.Collect()
        saveGraph theGraph outputGraphFileName
        printfn "Class graph saved to %s" outputGraphFileName
    with 
        | CallNumberError msg -> printfn "CallNumberError: %s" msg
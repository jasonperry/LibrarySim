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
let parseSeeAlso (datafield: MarcXmlType.Datafield) : CrossrefInfo = 
    (* let mutable desc =
        if datafield.Subfields.[0].Code = "i" then
            datafield.Subfields.[0].Value
        else "" *)
    let trimPunctuation (sfs : string) = 
        // TODO: strip parens also.
        // Order is important, there's '+,' in the data.
        if sfs.IndexOf ";" <> -1 then // when are there these? 
            sfs.[0..(sfs.IndexOf ";" - 1)]
        elif sfs.IndexOf "+" <> -1 then 
            sfs.[0..(sfs.IndexOf "+" - 1)]
        elif sfs.IndexOf "," <> -1 then 
            sfs.[0..(sfs.IndexOf "," - 1)]
        else sfs
    let mutable i = 0 
    let rec scanSubfields () =
        if i >= datafield.Subfields.Length then
            []
        else
            let sfi = datafield.Subfields.[i]
            match sfi.Code with
            | "i" -> 
                i <- i+1
                (sfi.Value, None, None) :: scanSubfields ()
            | "a" -> 
                let cnStartStr = trimPunctuation sfi.Value
                let cnEndStr = 
                    // Look for a following field "c", grab it and move the pointer.
                    if datafield.Subfields.Length > i+1 && datafield.Subfields.[i+1].Code = "c" then
                        i <- i+1 
                        trimPunctuation datafield.Subfields.[i].Value
                    else ""
                i <- i + 1
                let cnRange = 
                    try
                        let startCN = LCCN.parse cnStartStr     
                        let endCN = if cnEndStr = "" then startCN 
                                    else LCCN.parse cnEndStr
                        Some {startCN=startCN; endCN=endCN}
                    with CallNumberException errstr -> 
                        Logger.Error "CallNumberError in field 253: %s" errstr
                        None
                (sfi.Value, cnRange, None) :: scanSubfields ()
            | _ -> 
                Logger.Warning "Unknown 253 subfield code %s, value: %s" sfi.Code sfi.Value
                i <- i + 1
                (sfi.Value, None, None) :: scanSubfields ()
    scanSubfields () // Caller should log error if empty.

/// Add node to graph by calling SubjectGraph.insertNode with the 
/// CNRange comparison function. 
let insertNode (graph: SubjectGraph) node = 
    match npIndex.FindExact(node.subdividedName) with 
        | Some exactMatch -> 
            // Should this be done in the SubjectGraph insert code?
            Logger.Info "Subject %A already in index" node.name 
            exactMatch
        | None -> 
            match node.callNumRange with
            | Some cn -> 
                let isNarrower = fun n1 n2 -> CNRange.isSubRange n1.callNumRange.Value n2.callNumRange.Value
                SubjectGraph.insertNode isNarrower graph node 
            | None -> 
                Logger.Warning "No call number for %s, not inserting (for now)" node.name
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
        let mutable crossRefs = []
        callNumCount <- 0
        for controlfield in record.Controlfields do
            // Now getting this from control field instead of data field.
            if controlfield.Tag = "001" then
                controlNumber <- controlfield.Value.Split([|' '|]) |> String.concat ""
        for datafield in record.Datafields do
            if datafield.Tag = "153" then
                // DELETED: former attempt to use computation expressions
                // Note: Some have alt call numbers at "c". How to deal? or is it a coding error?
                // If it has a "z" entry for a table, skip it.
                if Option.isNone (getSingleSubfield datafield "z") then
                    cnRangeStr <- 
                        match getSingleSubfield datafield "a" with //lcCallNumStart with 
                            | Some startStr -> 
                                match getSingleSubfield datafield "c" with //lcCallNumEnd with
                                    | Some endStr -> Some (startStr + "-" + endStr)
                                    | None -> Some startStr
                            | None -> None 
                    // TODO: why are we just adding these? shouldn't we chain them properly?
                    subjectNames.AddRange(getAllSubfields datafield "h")
                    subjectNames.AddRange(getAllSubfields datafield "j")
                    callNumCount <- callNumCount + 1

            if datafield.Tag = "253" || datafield.Tag = "353" then // "See" cross reference
                crossRefs <- parseSeeAlso datafield
                if not crossRefs.IsEmpty then
                    // Possible to get 253 without 153? Yes
                    Logger.Info "Got 'see also' info for %A" cnRangeStr
                    crossRefCount <- crossRefCount + 1
        // End of field detection, filter out and assemble the record.
        if callNumCount = 0 || cnRangeStr.IsNone then
            withNoCallNum <- withNoCallNum + 1
            Logger.Error "No call number entry (153) or string for record %s" controlNumber
        else 
            recordsAdded <- recordsAdded + 1
            insertNode theGraph {
                uri = System.Uri ("http://knowledgeincoding.net/cnsubject/" + controlNumber)
                name = SubjectNode.joinSubjectName (List.ofSeq subjectNames)
                subdividedName = List.ofSeq subjectNames
                cnString = cnRangeStr; 
                callNumRange = 
                    match cnRangeStr with
                    | Some rstr -> 
                        try 
                            Some (CNRange.parse rstr)
                        with
                        | CallNumberException errstr -> 
                            Logger.Error "CNRange #%d: %s" recordCount errstr 
                            None
                    | None -> None
                broader = new List<SubjectNode>()
                narrower = new List<SubjectNode>()
                seeAlso = crossRefs
                books = new List<BookRecord>()
                booksUnder = 0
            } |> ignore
            // tried this way, not what we want...
            (* SubjectGraph.addSubject theGraph (SubjectNode.joinSubjectName (List.ofSeq subjectNames)) cnRangeStr
            |> ignore *)
        recordCount <- recordCount + 1
        if recordCount % 1111 = 0 then 
            Logger.Debug "============= %d\n" recordCount
    printfn "Processed %d records"  recordCount 
    printfn "          %d with no call numbers" withNoCallNum
    printfn "          %d with cross-refs" crossRefCount
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
        printfn "Resolving Cross-reference links..."
        SubjectGraph.updateCrossrefs theGraph
        saveGraph theGraph outputGraphFileName
        printfn "Class graph saved to %s" outputGraphFileName
    with // Aren't these caught above?
        | CallNumberException msg -> Logger.Error "CallNumberError: %s" msg
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

open BookTypes
open CallNumber
open SubjectGraph

let outputGraphFileName = "output/ClassGraph.sgb"

(* if fsi.CommandLineArgs.Length < 2 then
    printfn "Need MarcXML.gz file argument"
    exit(1)
let xmlfile = fsi.CommandLineArgs.[1] *)

[<Literal>]
let DATADIR = @"C:\Users\Jason\code\LibrarySim\SubjectGraph\indexdata\"
[<Literal>]
let XMLSAMPLE = DATADIR + "MarcRecordSample.xml"
/// Giving a constant file name initializes the type provider magic.
type Marc21ClassRecord = XmlProvider<XMLSAMPLE> 

let mutable nodeCount = 0

let npIndex = NamePrefixIndex.Create()

/// Populate a Classification subject node's parents and children, then add to graph.
/// NOW no longer populating. 
let insertNode (graph: SubjectGraph) node = 
    match npIndex.FindExact(node.subdividedName) with 
        | Some exactMatch -> 
            printfn "Subject %A already in index" node.name 
            exactMatch
        | None -> 
            SubjectGraph.insertNode graph node 
            node
            (* npIndex.Add node
            // assume each node added just once, so no add..TEST IT
            let parent =  npIndex.MaxPrefixMatch(node)
            // but will it 'wedge in'?? Number of children should be *reduced* as it goes along.
            let children = // OR just take existing children of parent? might be none.
                npIndex.MinExtensions node.subdividedName
                //let ch1 = npIndex.SingleExtensions node.subdividedName
                //if not ch1.IsEmpty then ch1
                //else npIndex.AllExtensions(node.subdividedName) 
            // Add parent
            match parent with 
                | Some pnode -> 
                    node.broader.Add(pnode)
                    // pnode.narrower.Add(node)
                | None -> ()
            // Add children and (don't) add this node as a parent of children.
            node.narrower.AddRange(children)
            // Let the Subject Graph code "wire up" everything else
            SubjectGraph.insertNode graph node 
            node *)
            // ?? Should I temporarily add to top-level, then remove?
            // ...no...how about add a flag to the node to indicate orphan?
            //  current solution: just find the top level in post-processing.


let getSingleSubfield (datafield : Marc21ClassRecord.Datafield) code = 
    (* Still awkward, but you can't return from a for loop. *)
    match Array.tryFindIndex (fun (sf : Marc21ClassRecord.Subfield) -> 
                              sf.Code = code)
                             datafield.Subfields with
        | Some i -> Some (datafield.Subfields.[i].Value)
        | None -> None

let getAllSubfields (datafield : Marc21ClassRecord.Datafield) code = 
    datafield.Subfields
    |> Array.filter (fun (sf : Marc21ClassRecord.Subfield) -> sf.Code = code) 
    |> Array.map (fun (sf : Marc21ClassRecord.Subfield) -> sf.Value)

let rec nextRecord (reader : XmlReader) = 
    if reader.Read() then
        match reader.NodeType with
            | XmlNodeType.Element -> 
                if reader.Name = "record" then
                    Some (Marc21ClassRecord.Parse (reader.ReadOuterXml()))
                else nextRecord reader
            | _ -> nextRecord reader
    else None

let readRecords (reader : XmlReader) = 
    seq {
        while (reader.Read()) do
            if reader.NodeType = XmlNodeType.Element then
                if reader.Name = "record" then
                    yield (Marc21ClassRecord.Parse (reader.ReadOuterXml()))
    }

/// Create SubjectNode objects and send them to insertNode.
let processClassRecords (records : Marc21ClassRecord.Record seq) = 
    let theGraph = SubjectGraph.emptyGraph()
    let mutable recordCount = 0
    let mutable withNoCallNum = 0
    let mutable callNumCount = 0
    for record in records do
        (*let recEnum = records.GetEnumerator()
        while recEnum.MoveNext() && recordCount < 20000 do 
        let record = recEnum.Current *)
        let mutable controlNumber = None
        let mutable cnRangeStr = None
        let subjectNames = new List<string>()
        callNumCount <- 0
        for datafield in record.Datafields do
            if datafield.Tag = 10 then
                // TODO: remove the space in the control number.
                controlNumber <- getSingleSubfield datafield "a"
                // debug for the "Arizona problem"
                if controlNumber.Value = "CF 94087466" then
                    Logger.Info "^^^ Adding Arizona"
                elif controlNumber.Value = "CF 99431990" then
                    Logger.Info "^^^ Adding Arizona child"
                elif controlNumber.Value = "CF 94087639" then
                    Logger.Info "^^^ Adding Arizona greatgrandchild"
                elif controlNumber.Value = "CF 99432215" then
                    Logger.Info "^^^ Adding Arizona grandchild"
                else ()
            if datafield.Tag = 153 then
                // Some have alt call numbers at "c"
                let tableField = getSingleSubfield datafield "z"
                let lcCallNumStart = // Option.lift2 (+) tableField (getSingleSubfield datafield "a")
                    match getSingleSubfield datafield "a" with 
                        | None -> None
                        | Some a -> match tableField with
                                    | None -> Some a
                                    | Some z -> Some (z+a)
                let lcCallNumEnd = // getSingleSubfield datafield "c"
                    match getSingleSubfield datafield "c" with 
                        | None -> None
                        | Some c -> match tableField with
                                    | None -> Some c
                                    | Some z -> Some (z+c)
                cnRangeStr <- 
                    // TODO: Monadize
                    match lcCallNumStart with 
                        | Some startStr -> 
                            match lcCallNumEnd with
                                | Some endStr -> Some (startStr + "-" + endStr)
                                | None -> Some startStr
                        | None -> None
                (*try 
                    LCCN.parse(lcCallNum.Value)  |> ignore // >>= ?
                with 
                    | _ -> printfn "Could not parse CN: %s" lcCallNum.Value *)
                subjectNames.AddRange(getAllSubfields datafield "h")
                subjectNames.AddRange(getAllSubfields datafield "j")
                callNumCount <- callNumCount + 1
        if callNumCount = 0 || cnRangeStr.IsNone then
            withNoCallNum <- withNoCallNum + 1
            Logger.Error <| "No call number entry (153) or string for record " + controlNumber.Value
        elif subjectNames.Count > 0 && subjectNames.[0].StartsWith("Table for") then
            // TODO: just try to parse the CN here, and skip if it fails.
            Logger.Info <| "Skipping 'table for' entry " + (defaultArg controlNumber "")
        else 
            insertNode theGraph {
                uri = System.Uri ("http://knowledgeincoding.net/cnsubject/" + controlNumber.Value);
                name = SubjectNode.joinSubjectName (List.ofSeq subjectNames); 
                subdividedName = List.ofSeq subjectNames;
                callNumRange = cnRangeStr; // TODO: parse here.
                broader = new List<SubjectNode>(); 
                narrower = new List<SubjectNode>();
                books = new List<BookRecord>();
                booksUnder = 0
            } |> ignore
            // tried this way, not what we want...
            (* SubjectGraph.addSubject theGraph (SubjectNode.joinSubjectName (List.ofSeq subjectNames)) cnRangeStr
            |> ignore *)
        recordCount <- recordCount + 1
        if recordCount % 1111 = 0 then 
            printfn "============= %d\n%A\n" recordCount subjectNames
    printfn "Processed %d records"  recordCount 
    printfn "          %d with no call numbers" withNoCallNum
    theGraph

// Basically a SubjectGraph, but I'll need to pull from it.
// How about: SubjectNodes, but a different structure based on
//  pulling out the subject headings, matching complex subject strings.
// Maybe it will obviate TopLevel Graph
// If I only use this and not the LCSH, have to make up my own IRIs.

let buildGraph gzfile = 
    let file = File.OpenRead(gzfile)
    let instream = new StreamReader(new GZipStream(file, mode=CompressionMode.Decompress))
    let reader = XmlReader.Create(instream)
    reader.MoveToContent() |> ignore // Can I avoid this or put it inside readRecords?
    try 
        let theGraph = processClassRecords (readRecords reader)
        SubjectGraph.makeTopLevel theGraph // mutates; guess it should be OO.
        printfn "** Nodes in Top Level: %d" theGraph.topLevel.Count
        reader.Close()
        instream.Close()
        // desperate attempt to reclaim memory before serialization.
        npIndex.Clear()
        System.GC.Collect()
        saveGraph theGraph outputGraphFileName
        printfn "Class graph saved to %s" outputGraphFileName
    with 
        | CallNumberError msg -> printfn "CallNumberError: %s" msg
    // Need to post-process graph, finding toplevel.

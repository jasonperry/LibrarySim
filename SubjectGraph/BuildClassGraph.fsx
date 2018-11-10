open System.IO.Compression
/// Build a graph out of the LOC Class MarcXML dataset.

//#I __SOURCE_DIRECTORY__
#I "C:\\Users\\Jason\\"
#r ".nuget/packages/FSharp.Data/3.0.0/lib/net45/FSharp.Data.dll"
#r "System.Xml.Linq.dll" // guess it's the net45 version.
#r "obj/Debug/net461/SubjectGraph.exe"

open System.Collections.Generic (* Always need this for lists. *)
open System.IO (* for file read and write *)
open System.IO.Compression
open System.Xml
open FSharp.Data

open BookTypes
open CallNumber
open SubjectGraph

let outputGraphFileName = "output/ClassGraph.sgb"

if fsi.CommandLineArgs.Length < 2 then
    printfn "Need MarcXML file argument"
    exit(1)
let xmlfile = fsi.CommandLineArgs.[1]

/// Giving a constant file name initializes the type provider magic.
type Marc21ClassRecord = XmlProvider<"MarcRecordSample.xml"> 

let theGraph = emptyGraph () 
let mutable nodeCount = 0

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

let processClassRecords (records : Marc21ClassRecord.Record seq) = 
    let mutable recordCount = 0
    let mutable withNoCallNum = 0
    let mutable callNumCount = 0
    for record in records do
        let mutable lcCallNum = None
        let subjectNames = new List<string>()
        callNumCount <- 0
        for datafield in record.Datafields do
            if datafield.Tag = 153 then
                lcCallNum <- getSingleSubfield datafield "a"
                subjectNames.AddRange (getAllSubfields datafield "h")
                subjectNames.AddRange (getAllSubfields datafield "j")
                callNumCount <- callNumCount + 1
        if callNumCount = 0 then
            withNoCallNum <- withNoCallNum + 1
            printfn "** No call number entry (153) for "
        recordCount <- recordCount + 1
        if recordCount % 47117 = 0 then 
            printfn "%A\n=============" subjectNames
    printfn "Processed %d records"  recordCount 
    printfn "          %d with no call numbers" withNoCallNum

// Basically a SubjectGraph, but I'll need to pull from it.
// How about: SubjectNodes, but a different structure based on
//  pulling out the subject headings, matching complex subject strings.
// Maybe it will obviate TopLevel Graph
// If I only use this and not the LCSH, have to make up my own IRIs.

// read in nodes

let file = File.OpenRead(xmlfile)
let instream = new StreamReader(new GZipStream(file, mode=CompressionMode.Decompress))
let reader = XmlReader.Create(instream)
reader.MoveToContent() // Can I avoid this or put it inside readRecords?

processClassRecords (readRecords reader)

// out of memory. Think I may have to stream read into elements, then parse
// individual records with the type provider.
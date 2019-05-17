/// Code for streaming and parsing MarcXml data.
module MarcXml

open System.IO // for file read and write 
open System.IO.Compression
open System.Xml
open FSharp.Data

[<Literal>]
let DATADIR = @"./indexdata/"
[<Literal>]
let XMLSAMPLE = DATADIR + "MarcRecordSample.xml"
/// Giving a constant file name initializes the type provider magic.
type MarcXmlType = XmlProvider<XMLSAMPLE> 

let getXmlReader filename =
    let file = File.OpenRead(filename)
    let instream = 
        if filename.EndsWith ".gz" then 
            new StreamReader(new GZipStream(file, mode=CompressionMode.Decompress))
        else new StreamReader(file)
    let reader = XmlReader.Create(instream)
    reader.MoveToContent() |> ignore
    reader  // Oh, but what about closing it?

/// Generator to parse individual MarcXML records from a stream.
let getRecordSeq (reader : XmlReader) = 
    seq {
        while (reader.Read()) do
            if reader.NodeType = XmlNodeType.Element then
                if reader.Name = "record" then
                    yield (MarcXmlType.Parse (reader.ReadOuterXml()))
    }

let getSingleSubfield (datafield : MarcXmlType.Datafield) code = 
    (* Still awkward, but you can't return from a for loop. *)
    match Array.tryFindIndex (fun (sf : MarcXmlType.Subfield) -> 
                                sf.Code = code)
                                datafield.Subfields with
        | Some i -> Some (datafield.Subfields.[i].Value)
        | None -> None

let getAllSubfields (datafield : MarcXmlType.Datafield) code = 
    datafield.Subfields
    |> Array.filter (fun (sf : MarcXmlType.Subfield) -> sf.Code = code) 
    |> Array.map (fun (sf : MarcXmlType.Subfield) -> sf.Value)

/// NOT USED. Earlier attempt to get XML by pieces?
let rec nextRecord (reader : XmlReader) = 
    if reader.Read() then
        match reader.NodeType with
            | XmlNodeType.Element -> 
                if reader.Name = "record" then
                    Some (MarcXmlType.Parse (reader.ReadOuterXml()))
                else nextRecord reader
            | _ -> nextRecord reader
    else None
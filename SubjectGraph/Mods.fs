module Mods

open System.IO // for file read and write 
open System.IO.Compression
open System.Xml
open FSharp.Data

[<Literal>]
let DATADIR = @"./indexdata/"
[<Literal>]
let MODSSAMPLE = DATADIR + "HarvardMODSResult.xml"

/// Start with a type for an entire result file, as I originally did for MarcXML.
type ModsType = XmlProvider<MODSSAMPLE> 

let getModsRecords (queryUrl : string) = 
    let result = ModsType.Load queryUrl
    let records = result.Items.Mods
    records

let modsToBookRecord = []

let test1 () = 
    let allRecords = getModsRecords (@"https://api.lib.harvard.edu/v2/items?name=schopenhauer&title=fourfold")
    for record in allRecords do
        printfn "%A" record
    printfn "*** %d records ***" allRecords.Length
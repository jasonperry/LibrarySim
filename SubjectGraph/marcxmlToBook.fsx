#r "../packages/FSharp.Data.2.4.6/lib/net45/FSharp.Data.dll"
#r "System.Xml.Linq.dll"
#load "CallNumber.fs"
#load "BookRecord.fs"
open BookRecord
open System.Collections.Generic (* Always need this for lists. *)
open System.IO (* for file read *)
open FSharp.Data

let xmlfile = fsi.CommandLineArgs.[1]

/// Giving a constant file name initializes the type provider magic.
type Marc21Slim = XmlProvider<"marcsample.xml"> 
// it just adds an 's'!

let log = printfn

let getSubfieldString (datafield : Marc21Slim.Datafield) code = 
    (* Still awkward, but you can't return from a for loop. *)
    match Array.tryFindIndex (fun (sf : Marc21Slim.Subfield) -> 
                              sf.Code.String = Some code)  
                             datafield.Subfields with
        | Some i -> Some (datafield.Subfields.[i].String.Value)
        | None -> None

(*    for subfield in datafield.Subfields do
        match subfield.Code.String with
            | Some a when a = code -> // Option.Value can throw. 
                Some subfield.String.Value
            | _ -> None *)

let processRecords (data : Marc21Slim.Collection) = 
    let books = new List<BookRecord>()
    let mutable totalRecords = 0
    let mutable withCallNum = 0
    let mutable withSubjects = 0
    for record in data.Records do
        printfn "------------"
        let mutable title = None
        let mutable authors = None (* Optional *)
        let mutable lcCallNum = None
        let subjects = new List<string>()
        for datafield in record.Datafields do
            printfn "Tag %d" datafield.Tag
            if datafield.Tag = 245 then
                printfn "Title Statement found"
                title <- getSubfieldString datafield "a"
                printfn ": %A" title
            elif datafield.Tag = 100 then
                printfn "Primary author found" (* TODO: dig out more authors *)
                authors <- getSubfieldString datafield "a"
                printfn ": %A" authors
            elif datafield.Tag = 650 then // can be multiples of these
                let subjTopic = getSubfieldString datafield "a"
                subjects.Add(subjTopic.Value)
            elif datafield.Tag = 50 then
                let cn = getSubfieldString datafield "a"
                printfn "Call Number: %s" cn.Value
                withCallNum <- withCallNum + 1
        printfn "Subjects: %A" subjects
        totalRecords <- totalRecords + 1
        if subjects.Count > 0 then
            withSubjects <- withSubjects + 1
    printfn "Processed %d records, %d with call numbers, %d with subjects" 
            totalRecords withCallNum withSubjects
    books



processRecords (Marc21Slim.Parse (File.ReadAllText xmlfile))

        (* printfn "Found tag %d" datafield.Tag *)
// printfn "%A" (data.GetSample().Values)
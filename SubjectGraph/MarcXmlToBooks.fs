/// Create and serialize BookRecord list from a MarcXML catalog.
module MarcXmlToBooks

(* //#I __SOURCE_DIRECTORY__
//#r "../packages/FSharp.Data.2.4.6/lib/net45/FSharp.Data.dll"
#I "C:\\Users\\Jason\\"
#r ".nuget/packages/FSharp.Data/3.0.0/lib/net45/FSharp.Data.dll"
#r "System.Xml.Linq.dll" // guess it's the net45 version.
#r "obj/Debug/net461/SubjectGraph.exe"
// #load "CallNumber.fs"
// #load "BookRecord.fs" // These are in the built library now. *)

open System.Collections.Generic // Always need this for lists.
open System.IO // for file read and write 
open System.IO.Compression
open System.Xml
open FSharp.Data
open System.Runtime.Serialization.Formatters.Binary

open Common
open MarcXml
open BookTypes
open CallNumber
open SubjectGraph

(* if fsi.CommandLineArgs.Length < 2 then
    printfn "Need MarcXML file argument"
    exit(1)
let xmlfile = fsi.CommandLineArgs.[1] *)

let OUTDIR = @"C:\Users\Jason\code\LibrarySim\SubjectGraph\output\"
let recordsFileName = OUTDIR + "gutenRecords.brb"

/// Giving a constant file name initializes the type provider magic.
// type Marc21Type = XmlProvider<"marcsample.xml"> 
// it just adds an 's'!
let log = printfn

// Return the MARC subfield of the given letter, if it exists.
(* let getSingleSubfield (datafield : Marc21Type.Datafield) code = 
    // Still awkward, but you can't return from a for loop.
    match Array.tryFindIndex (fun (sf : Marc21Type.Subfield) -> 
                              sf.Code.String = Some code)  
                             datafield.Subfields with
        | Some i -> Some (datafield.Subfields.[i].String.Value)
        | None -> None *)

(*    for subfield in datafield.Subfields do
        match subfield.Code.String with
            | Some a when a = code -> // Option.Value can throw. 
                Some subfield.String.Value
            | _ -> None *)

let processRecords (records : MarcXmlType.Record seq) = //(data : Marc21Type.Collection) = 
    let books = new List<BookRecord>()
    let mutable totalRecords = 0
    let mutable withCallNum = 0
    let mutable withDeweyNum = 0
    let mutable withSubjects = 0
    for record in records do
        printfn "------ %d ------" totalRecords
        let mutable controlNumber = None
        let mutable title = None
        let mutable subtitle = None
        let mutable authors = None 
        let mutable lcCallNum = None
        let mutable lcLetters = None
        let mutable link = None
        let subjects = new List<SubjectInfo>()
        for datafield in record.Datafields do
            if datafield.Tag = 10 then
                controlNumber <- getSingleSubfield datafield "a"
                // remove the space.
                match controlNumber with
                | Some s -> controlNumber <- Some (s.Replace (" ", ""))
                | None -> () // TODO: make up one if it's not there (not here, below)
            if datafield.Tag = 245 then
                // "Title Statement found: "
                title <- getSingleSubfield datafield "a"
                subtitle <- getSingleSubfield datafield "b"
                //printfn ": %A - %A" title subtitle
            elif datafield.Tag = 100 then
                //printf "Primary Author found: " // TODO: dig out more authors
                authors <- getSingleSubfield datafield "a"
                //printfn ": %A" authors
            // 150 is the topic heading for Marc21 Full. 
            // The gutenberg converter uses 653.
            elif (datafield.Tag = 650 || datafield.Tag = 653)
                 && Option.isSome (getSingleSubfield datafield "a")
            then // can be multiples of these
                let subjName = (getSingleSubfield datafield "a")
                                    .Value
                                    .Replace(" -- ", "--")
                // printfn "Subject (%d): %s" datafield.Tag subjName
                subjects.Add(
                    // I feel a little bit of a mismatch here. SubjectInfo seems really
                    // for the HTTP API, not for keeping with books.
                    { name = subjName; cnRange = None; uri = None; itemsUnder = 0})
            // some books have multiple call letters. This will take the last only.
            // TODO: make it a mutable list and append.
            elif datafield.Tag = 50 then
                let (sfa, sfb) = (getSingleSubfield datafield "a", getSingleSubfield datafield "b")
                let cn = sfa.Value + (sfb |? "")
                //printfn "Call Number: %s" cn
                try 
                    if cn.StartsWith "YA " then
                        // Actually, only skips if we skip adding books with no CN below.
                        printfn "Skipping special collection item %s" cn
                    else 
                        lcCallNum <- Some (LCCN.parse cn)
                        lcLetters <- Some (lcCallNum.Value.letters)
                        withCallNum <- withCallNum + 1
                with 
                    // If the call number is letters (gutenberg), detect and store.
                    | CallNumberError errorstr -> 
                        if LCCN.isCNLetters cn then 
                            lcLetters <- Some cn
                        else printfn "(!!) %s" errorstr
            elif datafield.Tag = 82 then
                let dcn = getSingleSubfield datafield "a"
                //printfn "Dewey Call number: %s" dcn.Value
                withDeweyNum <- withDeweyNum + 1
            elif datafield.Tag = 856 then
                link <- getSingleSubfield datafield "u"
        // printfn "Subjects: %A" subjects
        totalRecords <- totalRecords + 1
        if subjects.Count > 0 then
            withSubjects <- withSubjects + 1
        // Criterion for adding a book: currently that it has a parsed 
        // call number.
        if Option.isSome lcCallNum then
            printfn "Adding book: %s" title.Value
            books.Add({
                        Title = title.Value + 
                                if subtitle.IsSome then (" " + subtitle.Value) else "";
                        Authors = if authors.IsSome then authors.Value else "" ;
                        LCCallNum = lcCallNum;
                        LCLetters = lcLetters;
                        Subjects = List.ofSeq(subjects);
                        Uri = System.Uri ("http://knowledgeincoding.net/item/" + controlNumber.Value)
                        Link = link
            })
            // For now, only books with subjects.
    printfn "Processed %d records, %d call numbers, %d subjects," 
            totalRecords withCallNum withSubjects
    printfn "    %d with Dewey call numbers" withDeweyNum
    printfn "Generated %d book records" books.Count
    books

/// top-level function for the Gutenberg corpus; predates the "master subject
/// graph" approach.
let processBooks xmlFilename = 
    // let allbooks = processRecords (Marc21Type.Parse (File.ReadAllText xmlfile))
    let allbooks = processRecords (getRecordSeq (getXmlReader xmlFilename))

    let formatter = BinaryFormatter()
    let stream = new FileStream(recordsFileName, FileMode.Create)
    formatter.Serialize(stream, allbooks)
    stream.Close()
    printfn "Wrote records to file %s" recordsFileName
            (* printfn "Found tag %d" datafield.Tag *)
    // printfn "%A" (data.GetSample().Values)

/// top-level function for adding full MARC catalog records to the classification graph.
let addBooksToClassGraph (graph: SubjectGraph) xmlFilename = 
    processRecords (getRecordSeq (getXmlReader xmlFilename))
    |> Seq.filter (fun r -> Option.isSome r.LCCallNum)
    |> Seq.iter (fun br -> SubjectGraph.addItemByCallNumber graph br |> ignore)
    ()
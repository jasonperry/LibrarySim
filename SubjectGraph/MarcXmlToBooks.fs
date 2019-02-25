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
open FSharp.Data
open System.Runtime.Serialization.Formatters.Binary

open BookTypes
open CallNumber

(* if fsi.CommandLineArgs.Length < 2 then
    printfn "Need MarcXML file argument"
    exit(1)
let xmlfile = fsi.CommandLineArgs.[1] *)

let OUTDIR = @"C:\Users\Jason\code\LibrarySim\SubjectGraph\output\"
let recordsFileName = OUTDIR + "gutenRecords.brb"

/// Giving a constant file name initializes the type provider magic.
type Marc21Slim = XmlProvider<"marcsample.xml"> 
// it just adds an 's'!
let log = printfn

// Return the MARC subfield of the given letter, if it exists.
let getSubfieldString (datafield : Marc21Slim.Datafield) code = 
    // Still awkward, but you can't return from a for loop.
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
    let mutable withDeweyNum = 0
    let mutable withSubjects = 0
    for record in data.Records do
        printfn "------------"
        let mutable title = None
        let mutable subtitle = None
        let mutable authors = None 
        let mutable lcCallNum = None
        let mutable lcLetters = None
        let mutable link = None
        let subjects = new List<SubjectInfo>()
        for datafield in record.Datafields do
            //printfn "Tag %d" datafield.Tag
            if datafield.Tag = 245 then
                printfn "Title Statement found: "
                title <- getSubfieldString datafield "a"
                subtitle <- getSubfieldString datafield "b"
                printfn ": %A (%A)" title subtitle
            elif datafield.Tag = 100 then
                printf "Primary Author found: " // TODO: dig out more authors
                authors <- getSubfieldString datafield "a"
                printfn ": %A" authors
            // 150 is the topic heading for Marc21 Full. 
            // The gutenberg converter uses 653.
            elif datafield.Tag = 650 || datafield.Tag = 653 then // can be multiples of these
                let subjName = (getSubfieldString datafield "a")
                                    .Value
                                    .Replace(" -- ", "--")
                printfn "Subject (%d): %s" datafield.Tag subjName
                subjects.Add({ name = subjName; uri = None})
            // some books have multiple call letters. This will take the last only.
            // TODO: make it a mutable list and append.
            elif datafield.Tag = 50 then
                let cn = (getSubfieldString datafield "a").Value
                printfn "Call Number: %s" cn
                try 
                    lcCallNum <- Some (LCCN.parse cn)
                    lcLetters <- Some (lcCallNum.Value.letters)
                with 
                    // If the call number is letters (gutenberg), detect and store.
                    | CallNumberError errorstr -> 
                        if LCCN.isCNLetters cn then 
                            lcLetters <- Some cn
                        else printfn "(!!) %s" errorstr
                withCallNum <- withCallNum + 1
            elif datafield.Tag = 82 then
                let dcn = getSubfieldString datafield "a"
                printfn "Dewey Call number: %s" dcn.Value
                withDeweyNum <- withDeweyNum + 1
            elif datafield.Tag = 856 then
                link <- getSubfieldString datafield "u"
        // printfn "Subjects: %A" subjects
        totalRecords <- totalRecords + 1
        if subjects.Count > 0 then
            withSubjects <- withSubjects + 1
            books.Add({
                        Title = title.Value + 
                                if subtitle.IsSome then (" " + subtitle.Value) else "";
                        Authors = if authors.IsSome then authors.Value else "" ;
                        LCCallNum = lcCallNum;
                        LCLetters = lcLetters;
                        Subjects = List.ofSeq(subjects)
                        Link = link
            })
            // For now, only books with subjects.
    printfn "Processed %d records, %d call numbers, %d subjects," 
            totalRecords withCallNum withSubjects
    printfn "    %d with Dewey call numbers" withDeweyNum
    printfn "Generated %d book records" books.Count
    books
let processBooks xmlfile = 
    let allbooks = processRecords (Marc21Slim.Parse (File.ReadAllText xmlfile))
    //let allbooks = processRecords (Marc21Slim.Load(xmlfile))

    let formatter = BinaryFormatter()
    let stream = new FileStream(recordsFileName, FileMode.Create)
    formatter.Serialize(stream, allbooks)
    stream.Close()
    printfn "Wrote records to file %s" recordsFileName
            (* printfn "Found tag %d" datafield.Tag *)
    // printfn "%A" (data.GetSample().Values)
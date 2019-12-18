/// Create and serialize BookRecord list from a MarcXML catalog.
module MarcXmlToBooks

open System.Collections.Generic // Always need this for mutable lists.
open System.IO // for file read and write 
open System.IO.Compression
open System.Xml
open FSharp.Data

open Common
open MarcXml
open BookTypes
open CallNumber
open SubjectGraph

let OUTDIR = __SOURCE_DIRECTORY__ + @"/output/"
let recordsFileName = OUTDIR + "BookRecords.brb"

/// Giving a constant file name initializes the type provider magic.
// type Marc21Type = XmlProvider<"marcsample.xml"> 
// it just adds an 's'!

/// Find the first 4-digit year in a string from MARC field 260 subfield c.
let extract260cYear (field: string) = 
    let mutable starti = 0
    while starti < field.Length && not (System.Char.IsDigit field.[starti]) do
        starti <- starti + 1
    let mutable endi = starti
    while endi < field.Length && System.Char.IsDigit field.[endi] do
        endi <- endi + 1
    if endi - starti = 4 then
        Some (int field.[starti..endi-1])
    else
        None

// (Tail-)recursive version.
(*let extract260cYear (field: string) = 
    // I wrote a mutating version and it was harder to make correct!
    let rec extr acc i = 
        if i = field.Length then acc
        elif System.Char.IsDigit field.[i] then
            extr (acc + string field.[i]) (i+1)
        else
            if acc.Length > 0 then acc
            else extr acc (i+1)
    let yearstr = extr "" 0
    if yearstr.Length = 4 then
        Some (int yearstr)
    else
        None
*)

let processBookRecords (records : MarcXmlType.Record seq) = //(data : Marc21Type.Collection) = 
    let books = new List<BookRecord>()
    let mutable totalRecords = 0
    let mutable withCallNum = 0
    let mutable withDeweyNum = 0
    let mutable withSubjects = 0
    for record in records do
        printfn "------ %d ------" totalRecords
        let mutable controlNumber = null // only mandatory field, for now.
        let mutable title = None
        let mutable subtitle = None
        let mutable authors = None 
        let mutable lcCallNum = None
        let mutable lcLetters = None
        let mutable year = None
        let links = new List<string>()
        let subjects = new List<string * System.Uri option>()
        // Formerly got control number from datafield; seems more robust to 
        //  get it from the control field.
        for controlfield in record.Controlfields do
            if controlfield.Tag = "001" then
                controlNumber <- controlfield.Value
        for datafield in record.Datafields do
            if datafield.Tag = "245" then
                // "Title Statement found: "
                title <- getSingleSubfield datafield "a"
                subtitle <- getSingleSubfield datafield "b"
                printfn ": %A - %A" title subtitle
            elif datafield.Tag = "100" then
                printf "Primary Author found: " // TODO: dig out more authors
                authors <- getSingleSubfield datafield "a"
                printfn ": %A" authors
            elif datafield.Tag = "260" then
                match getSingleSubfield datafield "c" with
                | Some fieldstr -> 
                    year <- extract260cYear fieldstr 
                | None -> ()
            // 150 is the topic heading for Marc21 Full. 
            // The gutenberg converter uses 653.
            elif (datafield.Tag = "650" || datafield.Tag = "653")
                 && Option.isSome (getSingleSubfield datafield "a")
            then // can be multiples of these
                let subjName = (getSingleSubfield datafield "a")
                                    .Value
                                    .Replace(" -- ", "--")
                // printfn "Subject (%d): %s" datafield.Tag subjName
                subjects.Add (subjName, None)
            // some books have multiple call letters. This will take the last only.
            // TODO: make it a mutable list and append.
            elif datafield.Tag = "050" then
                let (sfa, sfb) = (getSingleSubfield datafield "a", getSingleSubfield datafield "b")
                let cn = sfa.Value + (sfb |? "")
                printfn "Call Number: %s" cn
                try 
                    lcCallNum <- Some (LCCN.parse cn)
                    lcLetters <- Some (lcCallNum.Value.letters)
                    withCallNum <- withCallNum + 1
                with 
                    // If the call number is letters (gutenberg), detect and store.
                    | CallNumberException errorstr -> 
                        if LCCN.isCNLetters cn then 
                            lcLetters <- Some cn
                        else printfn "(!!) %s" errorstr
            elif datafield.Tag = "82" then
                let dcn = getSingleSubfield datafield "a"
                //printfn "Dewey Call number: %s" dcn.Value
                withDeweyNum <- withDeweyNum + 1
            elif datafield.Tag = "856" then
                appendMaybe links (getSingleSubfield datafield "u")
        // endfor (record.Datafields)
        totalRecords <- totalRecords + 1
        if subjects.Count > 0 then
            withSubjects <- withSubjects + 1
        // Criterion for adding a book: that it has a title.
        if Option.isSome title then
            printfn "Constructing book record for: %s" title.Value
            let trimSlash (s : string) = 
                if s.EndsWith " /" then s.[0..(s.Length - 3)] else s
            books.Add({
                Title = trimSlash title.Value + 
                        (match subtitle with 
                         | Some st -> " " + trimSlash st
                         | None -> ""
                        )
                Authors = authors |? "" 
                LCCallNum = lcCallNum
                LCLetters = lcLetters
                Year = year
                Subjects = subjects
                // To be more general, do I only want a URI when I add it to the graph?
                Uri = System.Uri ("http://knowledgeincoding.net/item/" + controlNumber)
                Links = List.ofSeq links
            })
            // For now, only books with subjects.
    printfn "Processed %d records, %d parsed call numbers, %d subjects," 
            totalRecords withCallNum withSubjects
    printfn "    %d with Dewey call numbers" withDeweyNum
    printfn "Generated %d book records" books.Count
    books

/// Parse a Marc21Xml item collection into a list of BookRecords and serialize to disk.
let processBooks xmlFilename = 
    // let allbooks = processRecords (Marc21Type.Parse (File.ReadAllText xmlfile))
    let allbooks = processBookRecords (getRecordSeq (getXmlReader xmlFilename))
    // BookTypes.saveBooks allbooks recordsFileName
    //printfn "Wrote records to file %s" recordsFileName
    let bookDB = new BookTypes.BooksDB (OUTDIR + "books.sqlite")
    let numAdded = bookDB.AddBooks (List.ofSeq allbooks)
    printfn "Wrote %d books to database" numAdded

/// top-level function for parsing MarcXML item records and adding to the graph.
/// TODO: Move to SubjectGraph. No! just make it get the Subject URI.
let addBooksToClassGraph (graph: SubjectGraph) xmlFilename = 
    let skippedCallLetters = [ "YA"; ]
    processBookRecords (getRecordSeq (getXmlReader xmlFilename))
    // This is where we can only add books with parsed call numbers.
    |> Seq.filter 
        (fun r -> 
            Option.isSome r.LCCallNum 
            && 
            let cnLetters = r.LCCallNum.Value.letters
            if (List.contains cnLetters skippedCallLetters) then
                printfn "Skipping special collection item %s" cnLetters
                false
            else true)
    |> Seq.iter (fun br -> SubjectGraph.addItemByCallNumber graph br |> ignore)
    ()
#r "../packages/FSharp.Data.2.4.6/lib/net45/FSharp.Data.dll"
#r "System.Xml.Linq.dll"
#load "CallNumber.fs"
#load "BookRecord.fs"
#load "SparqlQuery.fs"
#load "Library.fs"

open System.IO // for file read and write 
open SubjectGraph
open System.Collections.Generic (* Always need this for lists. *)
open System.Runtime.Serialization.Formatters.Binary
open BookRecord

let ADD_BOOKS = true
let bookRecordsFileName = "records.brb"
let graphFileName = "graph.sgb"
let topLevelGraph = "TopLevelIndex.sgb"

// Load the top-level graph
let theGraph = 
    if topLevelGraph = "" then
        SubjectGraph.emptyGraph()
    else 
        SubjectGraph.loadGraph topLevelGraph
        

let booklist = 
    let booksFormatter = BinaryFormatter()
    let stream = new FileStream(bookRecordsFileName, FileMode.Open)
    let bl = booksFormatter.Deserialize(stream) // has type 'obj'
    stream.Close()
    bl :?> List<BookRecord> 

// TODO (Important): catch timeout error and pause for reconnect
//  (so we won't have to start all over.)
let success = Seq.mapi (fun i book -> // try 
                            printfn "Adding subjects for book number %d " i
                            addBookSubjects theGraph ADD_BOOKS book) 
                        booklist

printfn "Processed %d books, generating %d subject headings" 
        booklist.Count theGraph.uriIndex.Count
printfn "%d books not added (no subject found)" 
        <| Seq.length (Seq.filter (not) success)

// write to disk.
let graphFormatter = BinaryFormatter()
let stream = new FileStream(graphFileName, FileMode.Create)
graphFormatter.Serialize(stream, theGraph)
stream.Close()

printfn "Wrote subject graph file to %s" graphFileName


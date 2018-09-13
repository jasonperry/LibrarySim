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

let graphFileName = "graph.sgb"
let theGraph = emptyGraph()

let booklist = 
    let booksFormatter = BinaryFormatter()
    let stream = new FileStream("records.brb", FileMode.Open)
    let bl = booksFormatter.Deserialize(stream) // has type 'obj'
    stream.Close()
    bl :?> List<BookRecord> 

// TODO (Important): catch timeout error and pause for reconnect
//  (so we won't have to start all over.)
let success = Seq.mapi (fun i book -> // try 
                            printfn "Adding subjects for book number %d " i
                            addBookSubjects theGraph true book) 
                        booklist

printfn "Processed %d books, generating %d subject headings" 
        booklist.Count theGraph.subjectUriIndex.Count
printfn "%d books not added (no subject found)" 
        <| Seq.length (Seq.filter (not) success)

// write to disk.
let graphFormatter = BinaryFormatter()
let stream = new FileStream(graphFileName, FileMode.Create)
graphFormatter.Serialize(stream, theGraph)
stream.Close()

printfn "Wrote subject graph file to %s" graphFileName


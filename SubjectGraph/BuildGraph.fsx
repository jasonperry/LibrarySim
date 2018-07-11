#r "../packages/FSharp.Data.2.4.6/lib/net45/FSharp.Data.dll"
#r "System.Xml.Linq.dll"
#load "CallNumber.fs"
#load "BookRecord.fs"
#load "Library.fs"

open System.IO (* for file read and write *)
open SubjectGraph
open System.Collections.Generic (* Always need this for lists. *)
open System.Runtime.Serialization.Formatters.Binary
open BookRecord

let booklist = 
    let booksFormatter = BinaryFormatter()
    let stream = new FileStream("records.brb", FileMode.Open)
    let bl = booksFormatter.Deserialize(stream)
    stream.Close()
    bl :?> List<BookRecord> 

// TODO (Important): catch timeout error and pause for reconnect
//  (so we won't have to start all over.)
let success = Seq.mapi (fun i book -> // try 
                            printfn "Adding subjects for book number %d " i
                            addBookSubjects theGraph true book) 
                        booklist

printfn "%d books not added (no subject found)" 
        <| Seq.length (Seq.filter (not) success)

// write to disk.
let graphFormatter = BinaryFormatter()
let stream = new FileStream("graph.sgb", FileMode.Create)
graphFormatter.Serialize(stream, theGraph)
stream.Close()

printfn "Added %d books to graph, generating %d subject headings" 
        booklist.Count theGraph.subjectUriIndex.Count
/// Build a graph from a serialized set of BookRecords. DEFUNCT
module BuildFromBooks

(* #I __SOURCE_DIRECTORY__  // though it doesn't need any packages
#r "System.Xml.Linq.dll"
// Serializing from the object code solves the assembly problems! Woohoo!
#r "obj/Debug/net461/SubjectGraph.exe" *)

open System.IO // for file read and write 
open SubjectGraph
open System.Collections.Generic // Always need this for lists.
open System.Runtime.Serialization.Formatters.Binary
open BookTypes

/// Whether to add the books themselves, or just the discovered subjects.
let ADD_BOOKS = true

// Workaround for current directory problem in dotnet.
let CURDIR = @"C:\Users\Jason\code\LibrarySim\SubjectGraph\"

// let bookRecordsFileName = CURDIR + @"output\records.brb"
let graphFileName = CURDIR + @"output\graph.sgb"
let topLevelGraph = CURDIR + @"output\TopLevelIndex.sgb"

let buildGraph bookRecordsFileName = 
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
    saveGraph theGraph graphFileName

    printfn "Wrote subject graph file to %s" graphFileName


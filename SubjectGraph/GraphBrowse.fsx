/// Browse a serialized SubjectGraph using a textual interface. 

// All code libraries have to be loaded, but only things you use need to be opened.
#I __SOURCE_DIRECTORY__
#r "../packages/FSharp.Data.2.4.6/lib/net45/FSharp.Data.dll"
#r "System.Xml.Linq.dll"

#load "CallNumber.fs"
#load "BookRecord.fs"
#load "SparqlQuery.fs"
#load "Library.fs"

open System.Runtime.Serialization.Formatters.Binary
open System.IO (* for file read and write *)
open SubjectGraph

let graphFileName = 
    if Array.length fsi.CommandLineArgs > 0 then
        fsi.CommandLineArgs.[1]
    else "output/graph.sgb"

printfn "Loading subject graph file %s" graphFileName

let graph = 
    let booksFormatter = BinaryFormatter()
    let stream = new FileStream(graphFileName, FileMode.Open)
    let bl = booksFormatter.Deserialize(stream)
    stream.Close()
    bl :?> SubjectGraph.SubjectGraph

browseGraph graph
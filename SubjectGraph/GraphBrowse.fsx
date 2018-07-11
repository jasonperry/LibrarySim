// All code libraries have to be loaded, but only things you use need to be opened.
#r "../packages/FSharp.Data.2.4.6/lib/net45/FSharp.Data.dll"
#r "System.Xml.Linq.dll"

#load "CallNumber.fs"
#load "BookRecord.fs"
#load "Library.fs"

open System.Runtime.Serialization.Formatters.Binary
open System.IO (* for file read and write *)
open SubjectGraph

let graphFileName = fsi.CommandLineArgs.[1]
// let graphFileName = "gutengraph.sgb"

let graph = 
    let booksFormatter = BinaryFormatter()
    let stream = new FileStream(graphFileName, FileMode.Open)
    let bl = booksFormatter.Deserialize(stream)
    stream.Close()
    bl :?> SubjectGraph

browseGraph graph
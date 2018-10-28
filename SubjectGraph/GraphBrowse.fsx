/// Browse a serialized SubjectGraph using a textual interface. 

// All code libraries have to be loaded, but only things you use need to be opened.
#I __SOURCE_DIRECTORY__
// Serializing from the object code solves the assembly problems! Woohoo!
#r "obj/Debug/net461/SubjectGraph.exe"
#r "System.Xml.Linq.dll"

open System.IO (* for file read and write *)
open SubjectGraph

let graphFileName = 
    if Array.length fsi.CommandLineArgs > 0 then
        fsi.CommandLineArgs.[1]
    else "output/graph.sgb"

printfn "Loading subject graph file %s" graphFileName

let graph = loadGraph graphFileName

browseGraph graph
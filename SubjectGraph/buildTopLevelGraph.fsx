open System.Runtime.Serialization.Formatters.Binary
// Read in a completed top-level index .csv file, generate and save a SubjectGraph of it.
#I "C:\\Users\\Jason\\"
#r ".nuget/packages/FSharp.Data/3.0.0/lib/net45/FSharp.Data.dll"
#r "System.Xml.Linq.dll" 
// Serializing from the object code solves the assembly problems! Woohoo!
#r "obj/Debug/net461/SubjectGraph.exe"

// Rearranging the order like this fixed broken serialization
open SubjectGraph
open System.Collections.Generic
open BookRecord
let graphFileName = "output/TopLevelIndex.sgb"
[<Literal>] 
let CSVFILE = "../bookdata/TopLevelIndex.csv"

type LOCIndex = FSharp.Data.CsvProvider<CSVFILE, AssumeMissingValues=true>
let index = LOCIndex.Load(CSVFILE)

let theGraph = emptyGraph () 
let mutable nodeCount = 0

// TODO: addSubjectNode (uri, name, altnames, parents) - computes booksUnder from narrower

for row in index.Rows do
    let parents = List.filter ((<>) "") [row.Parent1; row.Parent2; row.Parent3]
    let altnames = List.filter ((<>) "") [row.Altlabel1; row.Altlabel2; row.Altlabel3]
    let node = {
        uri = Uri row.URI
        name = row.``Auth Label``
        callNumRange = if row.``Call Num`` = "" then None
                       else Some row.``Call Num``
        // should throw if parents don't exist (haven't been added)
        broader = List.map (fun u -> theGraph.uriIndex.[Uri u]) parents
        narrower = new List<SubjectNode>()
        books = new List<BookRecord>()
        booksUnder = 0
    }
    nodeCount <- nodeCount + 1
    printf "%d.." nodeCount
    // should really only have one for the top-level index. Test this.
    theGraph.uriIndex.Add(node.uri, node)
    for label in node.name :: altnames do
        if theGraph.subjectNameIndex.ContainsKey(label) then
            printfn "Warning: subject name '%s' already exists" label
            theGraph.subjectNameIndex.[label] <-
                node :: theGraph.subjectNameIndex.[label] 
        else
            theGraph.subjectNameIndex.Add(label, [node])
    if node.callNumRange.IsSome then
        let cn = node.callNumRange.Value
        if theGraph.cnIndex.ContainsKey(cn) then
            printfn "Warning: call number '%s' already exists" cn
            theGraph.cnIndex.[cn] <- node :: theGraph.cnIndex.[cn]
        else
            theGraph.cnIndex.Add(cn, [node])
    if node.broader.IsEmpty then 
        theGraph.topLevel.Add(node)
    else
        for p in node.broader do
            p.narrower.Add(node)

printfn "\nProcessed %d subject entries" nodeCount
printfn "Added %d top level entries" theGraph.topLevel.Count
printfn "Added %d different names" theGraph.subjectNameIndex.Count
printfn "Added %d call letters/ranges" theGraph.cnIndex.Count

// testing
//browseGraph theGraph

saveGraph theGraph graphFileName

printfn "Graph saved to %s" graphFileName
/// Read in a completed top-level index .csv file, generate and save a SubjectGraph of it.
module BuildTopLevel

(* #I "C:\\Users\\Jason\\"
#r ".nuget/packages/FSharp.Data/3.0.0/lib/net45/FSharp.Data.dll"
#r "System.Xml.Linq.dll" 
// Serializing from the object code solves the assembly problems! Woohoo!
#r "obj/Debug/net461/SubjectGraph.exe" *)

// Rearranging the order like this fixed broken serialization
open System.Runtime.Serialization.Formatters.Binary
open SubjectGraph
open System.Collections.Generic
open BookTypes
open CallNumber

let graphFileName = "output/TopLevelIndex.sgb"
// [<Literal>] 
// let DATADIR = @"./indexdata/" // OK for type provider on Windows, but not for running!
[<Literal>]
//let CSVFILE = @"indexdata/TopLevelIndex.csv"
let CSVFILE = @"indexdata/TopLevelIndex-toponly.csv"

type LOCIndex = FSharp.Data.CsvProvider<CSVFILE, AssumeMissingValues=true>

// TODO: addSubjectNode (uri, name, altnames, parents) - computes booksUnder from narrower
let buildGraph () = 
    let index = LOCIndex.Load(__SOURCE_DIRECTORY__ + "/" + CSVFILE)
    let theGraph = SubjectGraph.emptyGraph () 
    let mutable nodeCount = 1 // for top node.

    for row in index.Rows do
        let parents = List.filter ((<>) "") [row.Parent1; row.Parent2; row.Parent3]
        // TODO: Add alt name handling to main SubjectGraph code.
        let altnames = List.filter ((<>) "") [row.Altlabel1; row.Altlabel2; row.Altlabel3]
        let subjName = row.``Auth Label``
        let node = {
            uri = System.Uri row.URI
            name = subjName
            subdividedName = SubjectNode.splitSubjectName subjName
            cnString = if row.``Call Num`` = "" then None
                           else Some row.``Call Num``
            callNumRange = 
                if row.``Call Num`` = "" then None
                else 
                    // Avoid parsing because these are only partial CNs.
                    let cnStrings = row.``Call Num``.Split [|'-'|]
                    let startCN = LCCN.lettersOnlyCN cnStrings.[0] 
                    let endCN = 
                        if cnStrings.Length = 1 
                        then startCN
                        else LCCN.lettersOnlyCN cnStrings.[1]
                    Some (CNRange.create startCN endCN)
                                    
            // Don't need to add parents manually, done by call number!
            broader = new List<_>() // (List.map (fun u -> theGraph.uriIndex.[System.Uri u]) parents)
            narrower = new List<SubjectNode>()
            books = new List<BookRecord>()
            booksUnder = 0
        }
        nodeCount <- nodeCount + 1
        printf "%d.." nodeCount
        // insert by call number
        let isNarrower n1 n2 = CNRange.isSubRange n1.callNumRange.Value n2.callNumRange.Value
        SubjectGraph.insertNode isNarrower theGraph node
        // should really only have one for the top-level index. Test this.
        //theGraph.uriIndex.Add(node.uri, node)
        // Still do this manually for now?
        for label in node.name :: altnames do
            if theGraph.subjectNameIndex.ContainsKey(label) then
                printfn "Warning: subject name '%s' already exists" label
                theGraph.subjectNameIndex.[label] <-
                    node :: theGraph.subjectNameIndex.[label] 
            else
                theGraph.subjectNameIndex.Add(label, [node])
        (*if node.cnString.IsSome then
            let cn = node.cnString.Value
            if theGraph.cnIndex.ContainsKey(cn) then
                printfn "Warning: call number '%s' already exists" cn
                theGraph.cnIndex.[cn] <- node :: theGraph.cnIndex.[cn]
            else
                theGraph.cnIndex.Add(cn, [node])
        if node.broader.Count = 0 then 
            theGraph.topLevel.Add(node)
        else
            for p in node.broader do
                p.narrower.Add(node) *)

    printfn "\nProcessed %d subject entries" nodeCount
    // printfn "Added %d top level entries" theGraph.topLevel.Count
    printfn "Added %d different names" theGraph.subjectNameIndex.Count
    printfn "Added %d call letters/ranges" theGraph.cnIndex.Count
    theGraph
    
let writeTopLevelGraph () = 
    saveGraph (buildGraph ()) graphFileName
    printfn "Graph saved to %s" graphFileName
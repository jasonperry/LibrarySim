// Read in my LOC headings csv, query for subjects, print info

#r "../packages/FSharp.Data.2.4.6/lib/net45/FSharp.Data.dll"
#r "System.Xml.Linq.dll"
open FSharp.Data
#load "CallNumber.fs"  // you have to load all four.
#load "BookRecord.fs"
#load "SparqlQuery.fs"
open SparqlQuery
#load "Library.fs"

type LOCSubjs = CsvProvider<"../bookdata/MyLOC.csv">

let subjs = LOCSubjs.Load("../bookdata/MyLOC.csv")
let mutable rowCount = 0
let mutable foundCount = 0

/// query string for a label.
let makeLabelQuery lbl = 
    queryPrefix 
    + "SELECT ?subj ?label WHERE { \n"
    // + "?subj madsrdf:classification \"" + row.``Call Letters`` + "\" .\n"
    + "?subj madsrdf:authoritativeLabel \"" + lbl + "\"@en .\n"
    + "?subj madsrdf:isMemberOfMADSCollection lcsh:collection_LCSHAuthorizedHeadings .\n"
    // + "BIND(EXISTS{?subj rdf:type madsrdf:ComplexSubject} AS ?complex) .\n"
    + "} LIMIT 25\n"

// Split apart a classification name by its periods.
let splitClassification (s : string) = 
    Array.map (fun (s: string) -> s.Trim ()) (s.Split [|'.'|])
    |> List.ofArray

for row in subjs.Rows do
    rowCount <- rowCount + 1
    // Query for an LCSH subject associated with the call number.
    printfn "-- %s: %s" row.``Call Letters`` row.Classification
    let qstringCN = 
        queryPrefix 
        + "SELECT ?subj ?label WHERE { \n"
        + "?subj madsrdf:classification \"" + row.``Call Letters`` + "\" .\n"
        + "?subj madsrdf:authoritativeLabel ?label .\n"
        + "?subj madsrdf:isMemberOfMADSCollection lcsh:collection_LCSHAuthorizedHeadings .\n"
        // + "BIND(EXISTS{?subj rdf:type madsrdf:ComplexSubject} AS ?complex) .\n"
        + "} LIMIT 25\n"
    // printf "%s\n" qstring // debug
    let qresult = sparqlQuery qstringCN
    for res in qresult.results do
        printf "CN Result: %s : %s\n" res.["label"] res.["subj"] 
    if qresult.results.Length > 0 then
        foundCount <- foundCount + 1
    else 
        // look for the label itself.
        let qresult = sparqlQuery (makeLabelQuery row.Classification) 
        for res in qresult.results do
            printf "Label Result: %s: %s\n" row.Classification res.["subj"]
        if qresult.results.Length > 0 then
            foundCount <- foundCount + 1
        else
            // split the label and try again
            for lbl in splitClassification row.Classification do
                let qresult = sparqlQuery (makeLabelQuery lbl)
                for res in qresult.results do
                    printf "Partial Label Result: %s: %s\n" lbl res.["subj"]
                if qresult.results.Length > 0 then
                    foundCount <- foundCount + 1

// looks like I'll be pickling a SubjectNode list.
// Then I can add a 'load' method to Library.fs
printfn "Out of %d call letters, %d returned results" rowCount foundCount

// Also query by the subject name and see how many I get?
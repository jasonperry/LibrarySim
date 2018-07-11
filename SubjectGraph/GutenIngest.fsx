/// Currently useless--doing everything through MarcXML. 
#r "RDFSharp.dll"
#load "CallNumber.fs"
#load "BookRecord.fs"

open System.IO          (* directory traversal *)
open BookRecord
open RDFSharp.Model

let pathprefix = @"C:\Users\Jason\code\LibrarySim\bookdata\gutenberg\samplerdf\"
let samplefile = pathprefix + "pg13701.rdf"

let rec walkPath startPath = 
    seq {
        yield! Directory.GetFiles(startPath)
        for subDir in Directory.GetDirectories(startPath) do
            yield! walkPath subDir
    }

let readRDFFile fname = 
    printfn "Reading file %s" fname
    RDFGraph.FromFile(RDFModelEnums.RDFFormats.NTriples, fname)

let qSubjects = """SELECT ?subjectstring 
                   WHERE {
                       ?subj MemberOf LCSH.
                       ?subj rdf:value ?subjectstring.
                   } """ 

/// Parse a Project Gutenberg RDF. 
let gutenRDFtoBook (graph : RDFGraph) = 
    (* get dcterms prefix "http://purl.org/dc/terms/" *)
    let dcterms = "http://purl.org/dc/terms/"
    let rdf = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
    let subjs = graph.SelectTriplesByPredicate(new RDFResource(dcterms + "subject"))
    // oh, I have to downcast. Can give runtime errors.
    let subjtriples = [
        for triple in subjs do
            yield! (graph.SelectTriplesBySubject(triple.Object :?> RDFResource))
                         .SelectTriplesByPredicate(new RDFResource(rdf + "value"))
    ]
    [ for t in subjtriples -> (t.Object :?> RDFPlainLiteral).Value ] // getting ugly...

(* You don't need any "let _" in F#. *)
gutenRDFtoBook (readRDFFile (fsi.CommandLineArgs.[1]))
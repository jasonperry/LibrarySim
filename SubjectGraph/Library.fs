/// Constructing and querying a graph of LOC Subject Headings.
module SubjectGraph

open BookRecord

open System         (* console *)
open System.Net     (* for HTTP requests *)
open System.Web
open System.IO      (* StreamReader *)
open System.Xml.Linq
open System.Collections.Generic (* Dictionary, Mutable List *)
// open RDFSharp.Model

(* TODO: read this from an .ini file THAT ISN'T SOURCE CONTROLLED! *)
let endpoint = "http://35.202.98.137:3030/locsh"

// Used by all the functions that construct queries.
let queryPrefix = "PREFIX madsrdf: <http://www.loc.gov/mads/rdf/v1#>\n"
                  + "PREFIX lcsh: <http://id.loc.gov/authorities/subjects/>\n"

/// A lightweight URI wrapper to differentiate from strings. 
/// Hope the constructor won't interfere with other library code.
type BasicUri = Uri of string
    with member this.Value = match this with (Uri s) -> s
         member this.Wrapped = match this with (Uri s) -> "<" + s + ">"

(* TODO: Move SPARQL stuff into its own module. *)
(* ***************************************************************** *)

/// Type to store result of a SPARQL query as a list of (variable, value) maps.
/// Should work for any SELECT query.
type SparqlResult = {
    vars: string list;
    results: Map<string, string> list; 
} with 
    static member FromXml xmlstr = 
        let xname = XName.Get
        let srname name = XName.Get ("{http://www.w3.org/2005/sparql-results#}" + name)
        let doc = XElement.Parse xmlstr
        let varlist = (doc.Element (srname "head")).Elements (srname "variable")
                      |> List.ofSeq
        let resultlist = (doc.Element (srname "results")).Elements(srname "result")
                         |> List.ofSeq
        { 
        (* need to throw errors here? Should just return null or empty. *)
        vars = List.map 
                    (fun (elt: XElement) -> (elt.Attribute (xname "name")).Value) 
                    varlist
        (* Create a map for all the bindings in each result. *)
        results = List.map (fun (result: XElement) -> 
                                Seq.map (fun (binding: XElement) -> 
                                             ((binding.Attribute (xname "name")).Value,
                                              binding.Value)) 
                                        (result.Elements (srname "binding"))
                                |> Map.ofSeq )
                           resultlist }

/// The boilerplate for sending a query.
let sparqlQuery (queryString : string) = 
    let url = endpoint + "?query=" + HttpUtility.UrlEncode(queryString)
    let req = HttpWebRequest.Create(url) :?> HttpWebRequest
    req.Method <- "GET" (* Probably the default. *)
    req.ContentType <- "application/x-www-form-urlencoded"
    let resp = req.GetResponse() :?> HttpWebResponse
    let stream = resp.GetResponseStream ()
    let reader = new StreamReader(stream) 
    SparqlResult.FromXml (reader.ReadToEnd())
    (* parse to list of results, each with dict of variables with list of bindings?*)

(* ***************************************************************** *)
type SubjectNode = {
    uri : string;
    name : string; (* keep variant names in hash table *)
    callNumRange : string option;
    (* no explicit refs needed for these, F# uses reference semantics *)
    (* It's kind of ingenious that the upwards are immutable and the downwards aren't. *)
    broader : SubjectNode list;
    narrower : List<SubjectNode> (* mutable *)
}

/// Structures are mutable for a more update-based process.
/// But now I have to initialize it.
type SubjectGraph = {
    topLevel : List<SubjectNode>; (* maybe this should be immutable *)
    (* No refs! F# already uses reference semantics for setting equal. *)
    (* went back to lists. Even broader's won't actually be added twice *)
    cnIndex : Dictionary<string, SubjectNode list>;  (* maybe not unique *)
    subjectNameIndex : Dictionary<string, SubjectNode list>;
    subjectUriIndex : Dictionary<string, SubjectNode>; (* should be unique *)
}

/// For now, have a single graph for the module.
let theGraph = { 
    topLevel = new List<_> ();
    cnIndex = new Dictionary<_,_> ();
    subjectNameIndex = new Dictionary<_,_> ();
    subjectUriIndex = new Dictionary<_,_> ()
}

(* ***************************************************************** *)
let getVariantLabels subj = []

let getBroaderTerms (subj : BasicUri) = 
    let queryString = 
        queryPrefix
        + "SELECT ?broader ?label WHERE {\n"
        +      subj.Wrapped + " madsrdf:hasBroaderAuthority ?broader .\n"
        + "    ?broader madsrdf:isMemberOfMADSCollection lcsh:collection_LCSH_General .\n"
        + "    ?broader madsrdf:authoritativeLabel ?label .\n"
        + "} LIMIT 10 \n"
        (* and general collection *)
    printfn "%s" queryString
    let res = sparqlQuery queryString
    [ for bindings in res.results -> (bindings.["broader"], bindings.["label"]) ]
    // List.map (fun (k : Map<string,string>) -> "<" + k.["broader"] + ">") res.results

let getNarrowerTerms (subj : BasicUri) = 
    let queryString = 
        queryPrefix
        + "SELECT ?narrower WHERE {\n"
        +      subj.Wrapped + " madsrdf:hasNarrowerAuthority ?narrower .\n"
        + "    ?narrower madsrdf:isMemberOfMADSCollection lcsh:collection_LCSH_General \n"
        + "} LIMIT 200 \n"
    printfn "%s" queryString
    let res = sparqlQuery queryString
    (* thought: should I use the subject URI as my unique ID also? *)
    [ for bindings in res.results -> bindings.["broader"] ]
    // List.map (fun (k : Map<string,string>) -> "<" + k.["narrower"] + ">") res.results

/// Pull down all single-item data for a subject.
let pullSubjectData label = 
    (* TODO: Sanitize label? Or does Jena already do it? *)
    let queryString = 
        queryPrefix
        + "SELECT ?subj ?callNum WHERE { \n"
        + "?subj madsrdf:authoritativeLabel \"" + label + "\"@en .\n"
        + "?subj madsrdf:isMemberOfMADSCollection lcsh:collection_LCSHAuthorizedHeadings .\n"
        + "OPTIONAL { ?subj madsrdf:classification ?callNum }\n"
        + "} LIMIT 25\n" 
    printfn "%s" queryString           
    sparqlQuery queryString

/// Attempt to retrieve a subject heading identifier for a subject label.
/// As is, this works for complex subjects as well as topics.
let subjectForLabel label =
    let queryString = 
        queryPrefix
        + "SELECT ?subj WHERE { "
        + "?subj madsrdf:authoritativeLabel \"" + label + "\"@en. "
        + "?subj madsrdf:isMemberOfMADSCollection lcsh:collection_LCSHAuthorizedHeadings "
        + "} LIMIT 25"
    printfn "%s" queryString           
    (sparqlQuery queryString).results.[0].["subj"] (* stopgap *)

/// Try to find a topic heading for a complex subject. Maybe not necessary.
let topicForComplexSubject subjectString = None

// ** Broad directives **     
(* read catalog entries and create books.
 * pull subject strings out of books and pass to this. *)

(* Way to construct a call number range when there isn't one: take min and
 * max of narrower ones (assuming the letters match) *)

/// Add a heading record and all its broader subjects to the graph.
/// Modifies the graph and returns a node option with the new or found node.
let rec addSubject graph label =
    let qres = pullSubjectData label (* Is querying twice going on? *)
    if qres.results.IsEmpty then 
        printfn "%s" ("(**) Empty result for label: " + label)
        None  
    elif graph.subjectUriIndex.ContainsKey (qres.results.[0].["subj"]) then
        (* TODO: check if the label is already there too, and if not add it *)
        Some graph.subjectUriIndex.[qres.results.[0].["subj"]]
    else
        if qres.results.Length > 1 then
            printfn "(**) Warning: multiple results for \"%s\", taking only first" label
        let res = qres.results.[0]
        let subj = res.["subj"]
        // idea: broader immutable, narrower mutable. 
        let broaderSubjects = getBroaderTerms (Uri subj)
        (* Here's a line with two functional tricks in it. (>>) is "after", like (o). *)
        let broaderNodes = List.map (snd >> addSubject graph) broaderSubjects |> List.choose id
        (* LATER: if a "broader" jumps out of the call number range or doesn't have one,
         * add the call number topic as a parent (will need to pass call number up.)  *)
        let newNode = { 
            uri = subj;
            name = label; (* keep variant names in hash table *)
            callNumRange = if res.ContainsKey "callNum" (* is there a "dictopt" *)
                           then Some (res.["callNum"])
                           else None;
            broader = broaderNodes;
            narrower = new List<SubjectNode>(); 
        }
        for node in broaderNodes do
            node.narrower.Add newNode
        (* Update relevant graph indices. *)
        if broaderNodes.IsEmpty then
            (* check if it was already added *)
            if not (graph.subjectUriIndex.ContainsKey newNode.uri) then
                graph.topLevel.Add newNode
        if not (graph.subjectNameIndex.ContainsKey label) then
            graph.subjectNameIndex.Add (label, [newNode])
        else
            (* It's OK just to append because existing nodes won't get this far *)
            graph.subjectNameIndex.[label] <- (newNode :: graph.subjectNameIndex.[label])
        graph.subjectUriIndex.Add(newNode.uri, newNode)
        (* Lastly, return the new node. *)
        Some newNode

        // Then add this to the broader's narrower list. 
        (* Also a query to get variant names; add them to the hash table *)
        (* Maybe complex subjects only get added to the hash table, 
            because they're book-identifying? *)
    

/// Loop to browse a graph.
/// Currently only does a tree walk (no sideways links)
/// Later can parameterize by input/output functions for other interfaces.
let browseGraph (graph : SubjectGraph) = 
    (* Remember our path back to the root. *)
    let hist = new Stack<List<SubjectNode>> ()
    (* Might like to print out some level info. *)
    (* Should also be getting the book count underneath and skipping if there's just one 
       (to avoid long single chains) *)
    printfn "*** Top Level Subjects ***" (* Shouldn't be here... *)
    (* letrec is preferable to a while loop and mutable variable? *)
    let rec loop (currentList : List<SubjectNode>) = 
        (* When I go up, construct mutable list.. why not make an immutable list? *)
        List.iteri (fun i node -> printf "| %d. %s " i node.name) (List.ofSeq currentList)
        (*let mutable count = 0
        for node in currentList do 
            printf "| %d. %s " count node.name
            count <- count + 1 *)
        printf "\n Enter an index, 'u' to go up, or 'q' to quit: "
        let input = Console.ReadLine()
        let (|NumOpt|CharOpt|Fail|) (input : string) = 
            match System.Int32.TryParse input with
                | (true, i) -> NumOpt i
                | _ -> match System.Char.TryParse input with 
                           | (true, c) -> CharOpt c
                           | _ -> Fail
        match input with
            | NumOpt i -> 
                if 0 <= i && i < currentList.Count then
                    hist.Push currentList
                    printfn "*** Subheadings of %s ***" currentList.[i].name
                    loop currentList.[i].narrower
                else 
                    printfn "Index out of range"
                    loop currentList
            | CharOpt c ->
                match c with 
                    | 'q' -> ()
                    | 'u' -> 
                        if hist.Count = 0 then
                            printfn "Already at top level!"
                            loop currentList
                        else
                            loop <| hist.Pop ()
                    | _ -> 
                        printfn "Unrecognized option"
                        loop currentList
            | Fail -> 
                printfn "Unrecognized input"
                loop currentList
        (* active pattern to match and interpret? *)
    loop graph.topLevel
        
/// Add a book's subjects to a graph (and add the book too?)
let addBookSubjects (graph : SubjectGraph) (book : BookRecord) = None

// Maybe I won't need to use the RDF library at all. 
(*
let loadSchema filename = 
    // let g = RDFGraph.FromFile (RDFModelEnums.RDFFormats.RdfXml, filename)
    printf "did something."


let hello name =
    printfn "Hello %s" name
    // let rdffunc = new RDFSharp.Model.RDFGraph ()
    printf "made a graph!"
*)

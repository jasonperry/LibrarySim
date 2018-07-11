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

// If a module is a singleton, this is good, right?
module Logger = 
    type LogLevel = DEBUG | INFO | WARNING | ALERT | ERROR
    let mutable private _level = INFO
    let setLevel level = _level <- level
    let Debug s = 
        if _level <= DEBUG then
            printfn "[DEBUG] %s" s
    let Info s = 
        if _level <= INFO then
            printfn "[INFO] %s" s
    let Warning s = 
        if _level <= WARNING then
            printfn "[WARNING] %s" s
    let Alert s = 
        if _level <= ALERT then
            printfn "[ALERT] %s" s
    let Error s = 
        if _level <= ERROR then
            printfn "[ERROR] %s" s



(* TODO: read this from an .ini file THAT ISN'T SOURCE CONTROLLED! *)
let endpoint = "http://35.202.98.137:3030/locsh"

// Used by all the functions that construct queries.
let queryPrefix = "PREFIX madsrdf: <http://www.loc.gov/mads/rdf/v1#>\n"
                  + "PREFIX lcsh: <http://id.loc.gov/authorities/subjects/>\n"
                  + "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> \n"

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
                               resultlist 
        }

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
    name : string; // TODO: add variant names to subjectNameIndex *)
    callNumRange : string option
    (* no explicit refs needed for these, F# uses reference semantics *)
    (* It's kind of ingenious that the upwards are immutable and the downwards aren't. *)
    broader : SubjectNode list
    narrower : List<SubjectNode> (* mutable; new parents not added,
                                  * but children are *)
    books : List<BookRecord>
    mutable booksUnder : int  // to keep a count
}

/// Structures are mutable for a more update-based process
/// (and to avoid duplicating the whole thing.)
/// But now I have to initialize it.
type SubjectGraph = {
    topLevel : List<SubjectNode>; // maybe this should be immutable
    // went back to immutable lists. Even broader's won't actually be added twice.
    cnIndex : Dictionary<string, SubjectNode list>;  (* maybe not unique *)
    subjectNameIndex : Dictionary<string, SubjectNode list>;
    (* should be unique...hashset? *)
    subjectUriIndex : Dictionary<string, SubjectNode>; 
}

/// For now, have a single graph for the module.
let theGraph = { 
    // TODO: change to have a top node instead of a top level.
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
    Logger.Debug queryString
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
    Logger.Debug queryString
    let res = sparqlQuery queryString
    (* thought: should I use the subject URI as my unique ID also? *)
    [ for bindings in res.results -> bindings.["broader"] ]
    // List.map (fun (k : Map<string,string>) -> "<" + k.["narrower"] + ">") res.results

/// Pull down all single-item data for a subject.
let pullSubjectData (label: string) = 
    let queryString = 
        queryPrefix
        + "SELECT ?subj ?callNum ?complex WHERE { \n"
        + "?subj madsrdf:authoritativeLabel \"" + label + "\"@en .\n"
        + "?subj madsrdf:isMemberOfMADSCollection lcsh:collection_LCSHAuthorizedHeadings .\n"
        + "BIND(EXISTS{?subj rdf:type madsrdf:ComplexSubject} AS ?complex) .\n"
        + "OPTIONAL { ?subj madsrdf:classification ?callNum }\n"
        + "} LIMIT 25\n" 
    Logger.Debug queryString
    sparqlQuery queryString

/// Attempt to retrieve a subject heading identifier for a subject label.
/// As is, this works for complex subjects as well as topics. NOT CURRENTLY CALLED
(* let subjectForLabel label =
    let queryString = 
        queryPrefix // shouldn't I allow variant labels too, to match more? 
        + "SELECT ?subj WHERE { "
        + "?subj madsrdf:authoritativeLabel \"" + label + "\"@en. "
        + "?subj madsrdf:isMemberOfMADSCollection lcsh:collection_LCSHAuthorizedHeadings "
        + "} LIMIT 25"
    printfn "%s" queryString           
    (sparqlQuery queryString).results.[0].["subj"] // stopgap *)

/// Try to find a topic heading for a complex subject. Maybe still a good idea to implement!
let topicForComplexSubject subjectString = None

// ** Broad directives **     
(* read catalog entries and create books.
 * pull subject strings out of books and pass to this. *)

(* Way to construct a call number range when there isn't one: take min and
 * max of narrower ones (assuming the letters match) *)

/// Add a heading record and all its broader subjects to the graph.
/// Modifies the graph and returns a node option with the new or found node.
let rec addSubject graph (label: string) = // need based on CN as well as label?
    (* TODO: Sanitize label? Or does Jena already do it? *)
    let label = label.Replace("\"", "\\\"")
    let qres = pullSubjectData label (* Is querying twice going on? *)
    if qres.results.IsEmpty then 
        Logger.Warning ("Empty result for label: " + label)
        None  
    // TODO: handle multiple results (multiple subjs with same name...)
    elif graph.subjectUriIndex.ContainsKey (qres.results.[0].["subj"]) then
        // TODO: check if the label is already there too, and if not add it
        Some graph.subjectUriIndex.[qres.results.[0].["subj"]]
    else
        // Create new subject node.
        if qres.results.Length > 1 then
            Logger.Warning (sprintf "Multiple results for \"%s\", taking only first" label)
        let res = qres.results.[0]
        let subj = res.["subj"]
        // idea: broader immutable, narrower mutable. 
        let broaderSubjects = getBroaderTerms (Uri subj)
        // Filter the data - (>>) is "after", like (o). 
        let broaderNodes = List.map (snd >> addSubject graph) broaderSubjects 
                           |> List.choose id  // filters out empties.
        // LATER: if a "broader" jumps out of the call number range or doesn't have one,
        //  add the call number topic as a parent (will need to pass call number up.)  
        let newNode = { 
            uri = subj;
            name = label; (* keep variant names in hash table *)
            callNumRange = if res.ContainsKey "callNum" then (* is there a "dictopt" *)
                                Some (res.["callNum"])
                           else None;
            broader = broaderNodes;
            narrower = new List<SubjectNode>(); 
            books = new List<BookRecord>();
            booksUnder = 0
        }
        for node in broaderNodes do
            node.narrower.Add newNode
        // Update relevant graph indices. Maybe move to separate function
        if broaderNodes.IsEmpty then
            // check if it was already added 
            if not (graph.subjectUriIndex.ContainsKey newNode.uri) then
                // Temp: don't add complex nodes with no broader
                if res.["complex"] = "false" then 
                    graph.topLevel.Add newNode
                else
                    Logger.Warning (sprintf "Complex subject %s not added" newNode.name)
        // Add key and value to subject name index. TODO: add variants.
        if not (graph.subjectNameIndex.ContainsKey label) then
            graph.subjectNameIndex.Add (label, [newNode])
        else
            // It's OK just to append because existing nodes won't get this far
            graph.subjectNameIndex.[label] <- (newNode :: graph.subjectNameIndex.[label])
        // Add the new subject URI to that index.
        graph.subjectUriIndex.Add(newNode.uri, newNode)
        // If subject has a call number, add that to the index.
        if res.ContainsKey "callNum" then
            if graph.cnIndex.ContainsKey "callNum" then
                graph.cnIndex.["callNum"] <- newNode :: graph.cnIndex.["callNum"]
            else graph.cnIndex.Add ("callNum", [newNode])
        // Lastly, return the new node, in case it needs to be examined.
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
        // When I go up, construct mutable list.. why not make an immutable list?
        List.iteri (fun i node -> printf "| %d. %s " i node.name) (List.ofSeq currentList)
        (*let mutable count = 0
        for node in currentList do 
            printf "| %d. %s " count node.name
            count <- count + 1 *)
        printf "\n Enter an index, 'u' to go up, 't' to top, or 'q' to quit: "
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
                    // If the selected node has books, print them. Later, add options to this.
                    if not (Seq.isEmpty (currentList.[i].books)) then
                        printfn "\n**** Books for subject \"%s\" ****" currentList.[i].name
                        Seq.iter (fun (book: BookRecord) -> printfn "][ %s" book.Title) 
                                 currentList.[i].books
                    // Print the title before recursing.
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
                    | 't' -> loop graph.topLevel
                    | _ -> 
                        printfn "Unrecognized option"
                        loop currentList
            | Fail -> 
                printfn "Unrecognized input"
                loop currentList
        (* active pattern to match and interpret? *)
    loop graph.topLevel
        
/// Add a book's subjects to a graph (and add the book too?)
let addBookSubjects (graph : SubjectGraph) (addBook : bool) (book : BookRecord) =
    (* for each subject, add it, get node back *) 
    // need some logic: If at least one doesn't have broader...do something.
    let nodes = List.map (fun subj -> addSubject graph subj) book.Subjects
                |> List.choose id
    if addBook then
        List.iter (fun node -> node.books.Add(book)) nodes
    if nodes.IsEmpty then
        Logger.Alert (sprintf "No subject found for \"%s\"; book not added" book.Title)
        false
    else true
    // graph 

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

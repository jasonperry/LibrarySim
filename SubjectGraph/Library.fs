/// Constructing and querying a graph of LOC Subject Headings.
module SubjectGraph

open BookRecord

open System         (* console *)
open System.Collections.Generic (* Dictionary, Mutable List *)
open SparqlQuery

// open RDFSharp.Model

// Beginning of a logging framework. TODO: move to its own module
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


/// A lightweight URI wrapper to differentiate from strings. 
/// Hope the constructor won't interfere with other library code.
type BasicUri = Uri of string
    with member this.Value = match this with (Uri s) -> s
         member this.Wrapped = match this with (Uri s) -> "<" + s + ">"

type SubjectNode = {
    uri : BasicUri;
    name : string; // TODO: add variant names to subjectNameIndex. OK to keep this as canonical-only?
    callNumRange : string option
    // no explicit refs needed for these, F# uses reference semantics 
    // It's kind of ingenious that the upwards are immutable and the downwards aren't. 
    broader : SubjectNode list
    narrower : List<SubjectNode> // mutable; new parents not added, but children are
    books : List<BookRecord>
    mutable booksUnder : int  // to keep a count
}

/// Switched to a class, so it's easy to initialize with ().
(*
type SubjectGraph() =
    // Do I still want a single top node?
    member sg.topLevel : List<SubjectNode> = new List<_>()
    // May return multiple subjects for a call number.
    member sg.cnIndex : Dictionary<string, SubjectNode list> = 
        new Dictionary<_,_> ()
    member sg.subjectNameIndex : Dictionary<string, SubjectNode list> = 
        new Dictionary<_,_> ()
    // Should be unique key....hashset?
    member sg.subjectUriIndex : Dictionary<BasicUri, SubjectNode> =
        new Dictionary<_,_> ()
*)

/// Went back to records because I need to modify the mutable structures.
type SubjectGraph = {
    topLevel : List<SubjectNode>; 
    // went back to immutable lists. Even broaders won't actually be added twice.
    cnIndex : Dictionary<string, SubjectNode list>;  // maybe not unique 
    subjectNameIndex : Dictionary<string, SubjectNode list>;
    // should be unique...hashset? Can we make out of BasicURI...yes.
    uriIndex : Dictionary<BasicUri, SubjectNode>; 
} 

let emptyGraph () =  { 
    // TODO: change to have a top node instead of a top level.
    topLevel = new List<_> (); // Later: read from pre-generated list.
    cnIndex = new Dictionary<_,_> ();
    subjectNameIndex = new Dictionary<_,_> ();
    uriIndex = new Dictionary<_,_> ()
}


(* ***************************************************************** *)

// TODO: actually this will just read in a graph file outputted by a script.
let buildTopLevelLOC = None
    // read in LCC subjects from file (into map?)
    // look it up.

let getVariantLabels subj = []

// Get a broader topic (probably only one) based on LCC.
let getBroaderLCC callNum = []

/// Return true if 1st URI is higher in the graph than the 2nd.
let rec isBroaderThan (graph: SubjectGraph) (uri1: BasicUri) (uri2: BasicUri) = 
    // ? should it accept a node? It should be one-to-one, but...
    let (node1, node2) = graph.uriIndex.[uri1], 
                         graph.uriIndex.[uri2]
    if List.isEmpty (node2.broader) then
        false
    elif List.contains node1 node2.broader then
        true
    else 
        List.exists (fun n -> isBroaderThan graph uri1 n.uri) node2.broader

let getBroaderTerms (subj : BasicUri) = 
    let queryString = 
        queryPrefix
        + "SELECT ?broader ?label WHERE {\n"
        +      subj.Wrapped + " madsrdf:hasBroaderAuthority ?broader .\n"
        + "    ?broader madsrdf:isMemberOfMADSCollection lcsh:collection_LCSH_General .\n"
        + "    ?broader madsrdf:authoritativeLabel ?label .\n"
        + "} LIMIT 10 \n"
        // and general collection?
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
let querySubjectData (label: string) = 
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

/// Try to find a topic heading for a complex subject. 
/// Probably by splitting it by the -- separators. But how to tell which one
///   is the 'fundamental' subject?
/// Maybe still a good idea to implement!
let topicForComplexSubject subjectString = None

// ** Broad directives **     
(* read catalog entries and create books.
 * pull subject strings out of books and pass to this. *)

(* Way to construct a call number range when there isn't one: take min and
 * max of narrower ones (assuming the letters match) *)

/// Add a heading record and all its broader subjects to the graph.
/// Modifies the graph and returns a node option with the new or found node.

let (|?) = defaultArg
let newSubjectUri (graph: SubjectGraph) label callLetters = 
    // TODO: make sure we don't repeat, have a counter in the graph to keep track.
    // call num then five digits? count backwards?
    Uri <| "http://knowledgeincoding.net/classif/" + (callLetters |? "XX") + label

// Take a completed subject entry and update other graph structures with its information.
let updateGraph graph (newNode: SubjectNode) = 
    // 1. Add it as children of all its parents.
    for node in newNode.broader do
        node.narrower.Add newNode
    (* if newNode.broader.IsEmpty then
        // check if it was already added 
        if not (graph.uriIndex.ContainsKey newNode.uri) then
            // Temp: don't add complex nodes with no broader
            if res.["complex"] = "false" then 
                graph.topLevel.Add newNode
            else 
                Logger.Warning (sprintf "Complex subject %s not added" newNode.name) *)
    // 2. Add key and value to subject name index. TODO: add variants.
    if not (graph.subjectNameIndex.ContainsKey newNode.name) then
        graph.subjectNameIndex.Add (newNode.name, [newNode])
    else
        // It's OK just to append because we know it's a new node.
        graph.subjectNameIndex.[newNode.name] <- (newNode :: graph.subjectNameIndex.[newNode.name])
    // 3. Add the new subject URI to that index.
    graph.uriIndex.Add(newNode.uri, newNode)
    // 4. If subject has a call number, add that to the index.
    // NOT APPROPRIATE if it's just letters. TODO: add test.
    (*if newNode.callNumRange.IsSome then // res.ContainsKey "callNum" then
        let cn = newNode.callNumRange.Value
        if graph.cnIndex.ContainsKey cn then
            graph.cnIndex.[cn] <- newNode :: graph.cnIndex.[cn]
        else graph.cnIndex.Add (cn, [newNode]) *)

let rec addSubjectByCN (graph: SubjectGraph) (label: string) (callLetters : string option) =
    // TODO: get more clear about when/how to clean the label.
    let label = label.Replace("\"", "\\\"")
    // 1. Try to get LCSH record for the subject label.
    let qres = querySubjectData label  
    // 2. Handle two cases where the subject can already be in the graph.
    if (not qres.results.IsEmpty) && graph.uriIndex.ContainsKey (Uri qres.results.[0].["subj"]) then
        Logger.Info <| "Subject " + label + " already in graph"
        Some graph.uriIndex.[Uri qres.results.[0].["subj"]]
    elif qres.results.IsEmpty && graph.subjectNameIndex.ContainsKey label then
        // TODO: if multiple matches, disambiguate by call number.
        Logger.Info <| "Subject label " + label + " already in graph"
        Some graph.subjectNameIndex.[label].[0] // bookAdd worries about being broader?
    // 3. Build a new node.
    else 
        Logger.Info <| "Building new subject node for " + label
        let uri = 
            if (not qres.results.IsEmpty) then 
                Uri qres.results.[0].["subj"]
            else 
                newSubjectUri graph label callLetters
        let parents = 
            if callLetters.IsSome && graph.cnIndex.ContainsKey callLetters.Value then
                // TODO: may want to do "chopping off" to get more depth
                graph.cnIndex.[callLetters.Value]
            (* elif (not qres.results.IsEmpty) then
                let broaderSubjects = getBroaderTerms uri
                // ISSUE: these might be broader than our top-level. 
                // ISSUE: Need to check/update call number.
                List.map (fun br -> addSubjectByCN graph (snd br) callLetters) broaderSubjects 
                    |> List.choose id  // filters out empties.
                // still recursively add? Yes, but it should stop now!!
                //  what if it has a call number?  *)
            else
                Logger.Warning 
                    ("Could not find parents for subject '" + label + "' - subject is orphan")
                []  // Hey! I could still add "orphan" subject and try to find their parents later!
                // or still might want to return None for now, to see how I'm doing.
        if parents.Length > 5 then
            Logger.Warning <| "Subject has " + string(parents.Length) + " direct parents."
            Logger.Warning <| sprintf "%A" (List.map (fun (n:SubjectNode) -> n.name) parents)
        let newNode = { 
            uri = uri;
            name = label; (* keep variant names in hash table *)
            callNumRange = if (not qres.results.IsEmpty) && qres.results.[0].ContainsKey "callNum" then 
                                Some (qres.results.[0].["callNum"])
                           else callLetters;
            broader = parents;
            narrower = new List<SubjectNode>(); 
            books = new List<BookRecord>();
            booksUnder = 0
        }
        updateGraph graph newNode // fill in narrower and indexes.
        if parents.IsEmpty then None else Some newNode
    
(* let rec addSubject (graph: SubjectGraph) (label: string) (callLetters : string option) = 
    // TODO: Sanitize label? Or does Jena already do it? 
    let label = label.Replace("\"", "\\\"")
    let qres = pullSubjectData label // Is querying twice going on? 
    let uri = if qres.results.IsEmpty then 
        Logger.Warning ("Empty result for label: " + label)
        // TODO: Add to set of un-found subjects, to examine later.
        None  
    // TODO: handle multiple results (multiple subjs with same name...)
    elif graph.uriIndex.ContainsKey (Uri qres.results.[0].["subj"]) then
        // TODO: check if the label is already there too, and if not add it
        Some graph.uriIndex.[Uri qres.results.[0].["subj"]]
    else
        // Create new subject node.
        if qres.results.Length > 1 then
            Logger.Warning (sprintf "Multiple results for \"%s\", taking only first" label)
        let res = qres.results.[0]
        let subj = Uri res.["subj"]
        // idea: broader immutable, narrower mutable.
        // NEW: If "have a call number" = getBroaderLOC else ...
        //   so it will keep going until we have a call number!
        let broaderSubjects = getBroaderTerms subj
        // Recursively add the broader subjects. 
        let broaderNodes = List.map (fun br -> addSubject graph (snd br) callLetters) broaderSubjects 
                           |> List.choose id  // filters out empties.
        // LATER: if a "broader" jumps out of the call number range or doesn't have one,
        //  add the call number topic as a parent (will need to pass call number up.)  
        let newNode = { 
            uri = subj;
            name = label; // keep variant names in hash table 
            callNumRange = if res.ContainsKey "callNum" then // is there a "dictopt"?
                                Some (res.["callNum"])
                           else None;
            broader = broaderNodes;
            narrower = new List<SubjectNode>(); 
            books = new List<BookRecord>();
            booksUnder = 0
        }
        updateGraph graph newNode
        // Lastly, return the new node, in case it needs to be examined.
        Some newNode *)

        // Then add this to the broader's narrower list. 
        (* Also a query to get variant names; add them to the hash table *)
        (* Maybe complex subjects only get added to the hash table, 
            because they're book-identifying? *)
    
/// Add a book's subjects to a graph (and the book too if selected)
let addBookSubjects (graph : SubjectGraph) (addBook : bool) (book : BookRecord) =
    // for each subject, add it, get node back 
    let nodes = List.map (fun subj -> addSubjectByCN graph subj book.LCLetters) book.Subjects
                |> List.choose id
    Logger.Info <| "Finished adding subjects for book \"" + book.Title + "\""
    // If we're storing books too, add it and update the counts
    if addBook then
        let rec updateCounts node = 
            node.booksUnder <- node.booksUnder + 1
            printf "."
            List.iter updateCounts node.broader
        for node in nodes do
            // Only add a book under a node if there's no narrower one.
            if not (List.exists (fun n -> isBroaderThan graph node.uri n.uri) nodes) then
                printfn "Adding book under node %s" node.name
                node.books.Add(book)
                updateCounts node
    if nodes.IsEmpty then
        Logger.Alert (sprintf "No subject found for \"%s\"; book not added" book.Title)
        false
    else true

/// Loop to browse a graph.
/// Currently only does a tree walk (no sideways links)
/// Later can parameterize by input/output functions for other interfaces.
let browseGraph (graph : SubjectGraph) = 
    (* Remember our path back to the root. *)
    let hist = new Stack<List<SubjectNode>> ()
    (* Might like to print out some level info. *)
    (* Should also be getting the book count underneath and skipping if there's just one 
       (to avoid long single chains) *)
    printfn "*** Top Level Subjects ***" // Shouldn't be here... 
    // letrec is preferable to a while loop and mutable variable? 
    let rec loop (currentList : List<SubjectNode>) = 
        // When I go up, construct mutable list.. why not make an immutable list?
        List.iteri (fun i node -> 
                        printf "| %d. %s " i node.name
                        if node.booksUnder > 0 then
                            printf "(%d)" node.booksUnder
                   ) 
                   (List.ofSeq currentList)
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
                    printfn ""
                    // Print the title before recursing.
                    if not (currentList.[i].narrower.Count = 0) then
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

// deserialize. The opens are a hint that maybe this should go elsewhere.

open System.IO // for file read and write 
open System.Runtime.Serialization.Formatters.Binary

let loadGraph graphFileName = 
    let booksFormatter = BinaryFormatter()
    let stream = new FileStream(graphFileName, FileMode.Open)
    let bl = booksFormatter.Deserialize(stream)
    stream.Close()
    bl :?> SubjectGraph
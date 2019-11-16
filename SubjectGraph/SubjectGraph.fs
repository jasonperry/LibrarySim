/// Constructing and querying a graph of LOC Subject Headings.
module SubjectGraph

open System
open System.Collections.Generic

open Common
open BookTypes
open CallNumber
open SparqlQuery


/// Cross-reference information list for a subject node.
/// TODO: Maybe move to SubjectGraph.
type CrossrefInfo = (string * LCCNRange option * System.Uri option) list

type SubjectNode = {
    uri : Uri;
    name : string; // TODO: add variant names to subjectNameIndex. OK to keep this as canonical-only?
    subdividedName : string list;
    callNumRange : LCCNRange option;
    cnString : string option; // just as a backup
    // I thought it was clever that the upwards are immutable and the downwards aren't. 
    broader : List<SubjectNode>; // SubjectNode list;  So sad, had to make it mutable...
    narrower : List<SubjectNode>; // mutable; new parents not added, but children are
    mutable seeAlso: CrossrefInfo;
    books : List<BookRecord>;
    mutable booksUnder : int  // to keep a count
}

/// Utility functions for individual SubjectNodes.
module SubjectNode =
    // TODO: These are now specific to subdividedNames, so go elsewhere?
    let isNarrower node1 node2 = 
        isStrictPrefix node2.subdividedName node1.subdividedName
        // TODO: Will I need to add CN-based later, or will this always suffice?
        // More fundamental question: are parents in the graph always broader?
    let isBroader node1 node2 = 
        isStrictPrefix node1.subdividedName node2.subdividedName
    let joinSubjectName slist = List.reduce (fun l r -> l + "--" + r) slist
    let splitSubjectName (name : string) = 
        // The character '@' doesn't exist in the Guten or LOC Class datasets.
        name.Replace(" -- ", "--").Replace("--","@").Split([|'@'|])
        |> Array.filter (fun s -> s <> "")
        |> List.ofArray
    let toSubjectInfo node = 
        {uri = Some node.uri; cnRange = node.callNumRange; 
         name = node.name; itemsUnder = node.booksUnder}

/// A class with methods for finding subjects by prefix. Not currently used in LCCN-based graph.
type NamePrefixIndex = private {
    theMap : Dictionary<string, List<SubjectNode>>
} with 
    static member Create () = { theMap = new Dictionary<string, List<SubjectNode>>() }
    member this.FindExact subjList = 
        try
            List.ofSeq this.theMap.[List.head subjList]
            |> List.find (fun nd -> List.tail nd.subdividedName = List.tail subjList)
            |> Some
        with 
            | _ -> None
    member this.Add node = 
        // open List // local open would be nice.
        let subjHead = List.head node.subdividedName
        if not(this.theMap.ContainsKey(subjHead)) then
            this.theMap.Add(subjHead, new List<_>())
        this.theMap.[subjHead].Add(node)
    member this.MaxPrefixMatch node =
        let subjHead = List.head node.subdividedName
        let subjTail = List.tail node.subdividedName
        if not(this.theMap.ContainsKey(subjHead)) then None
        else 
            let matchCandidates = 
                this.theMap.[subjHead]
                |> Seq.filter (fun nd -> isStrictPrefix (List.tail nd.subdividedName) subjTail)
            if Seq.isEmpty matchCandidates then None
            else Seq.maxBy (fun nd -> nd.subdividedName.Length) matchCandidates
                |> Some
    /// Return a list of the minimum extensions of a subject.
    member this.MinExtensions segments = 
        let subjHead = List.head segments
        if not(this.theMap.ContainsKey(subjHead)) then []
        else
            let allExtensions = 
                this.theMap.[subjHead]
                    |> Seq.filter (fun nd -> 
                                   isStrictPrefix segments nd.subdividedName)
            if Seq.isEmpty allExtensions then []
            else
                let minLength = (Seq.minBy (fun nd -> Seq.length nd.subdividedName) allExtensions)
                                    .subdividedName.Length
                Seq.filter (fun (nd : SubjectNode) -> 
                            nd.subdividedName.Length = minLength) allExtensions
                |> List.ofSeq
    /// Try 3: return a list of all current children of a subdivided name. 
    member this.FindChildren segments = 
        if not (this.theMap.ContainsKey(List.head segments)) then []
        else
            let allExtensions = 
                this.theMap.[List.head segments]
                    |> Seq.filter (fun nd -> 
                                   isStrictPrefix segments nd.subdividedName)
            let childList = new List<_>(allExtensions)
            if Seq.isEmpty childList then []
            else 
                // Tricky part: remove all extensions that are extensions of another one.
                let toRemove = new List<_>()
                for nodei in childList do
                    for nodej in childList do  // checks both directions
                        if isStrictPrefix nodei.subdividedName nodej.subdividedName then
                            toRemove.Add nodej
                Seq.iter (childList.Remove >> ignore) 
                         toRemove
                List.ofSeq childList

    /// Return a list of all nodes with names that are extensions of the given 
    /// one. May be deprecated for MinExtensions.
    member this.AllExtensions segments = 
        let subjHead = List.head segments
        let subjTail = List.tail segments
        if not(this.theMap.ContainsKey(subjHead)) then []
        else
            this.theMap.[subjHead]
            |> Seq.filter (fun nd -> isStrictPrefix (subjHead::subjTail) nd.subdividedName)
            |> List.ofSeq
    /// Return a list of all nodes with names that are one segment longer than 
    /// the given one. May be deprecated for MinExtensions.
    member this.SingleExtensions subdividedName = 
        let subjHead = List.head subdividedName
        let subjTail = List.tail subdividedName
        if not(this.theMap.ContainsKey(subjHead)) then []
        else
            this.theMap.[subjHead]
            |> Seq.filter (fun nd -> 
                List.length nd.subdividedName = (List.length subdividedName + 1)
                && isStrictPrefix (subjHead::subjTail) nd.subdividedName)
            |> List.ofSeq
    member this.Iterate = seq { for item in this.theMap.Values do
                                    yield! item }
    /// Use this to avoid storing the index in a finished graph. (not as nice 
    /// a solution as a new type or custom serialization.)
    member this.Clear () = this.theMap.Clear()

(* End class NamePrefixdex **************************************************)

/// Search algorithm for CN Index. Hopefully obsolete, all tree-based now.
(* module CNIndex = 
    let rec mostSpecificMatch (cnIndex: Dictionary<LCCNRange, SubjectNode list>) (cn: LCCNRange) = 
        if cnIndex.ContainsKey cn 
        then Some cnIndex.[cn]
        else 
            let shortenedcn = LCCN.shortenByOneField cn
            if shortenedcn = cn then None
            else mostSpecificMatch cnIndex shortenedcn *)

/// SubjectGraph record type.
type SubjectGraph = {
        // I tried making it a class but then couldn't modify the mutable fields.
        // topLevel : List<SubjectNode>; 
        topNode : SubjectNode;
        // went back to immutable lists. Even broaders won't actually be added twice.
        // UPDATE: used to be a list, but now I want to assume it's unique. 
        cnIndex : Dictionary<LCCNRange, SubjectNode>;  
        subjectNameIndex : Dictionary<string, SubjectNode list>;
        subjectPrefixIndex : NamePrefixIndex; // hopefully obsolete.
        // should be unique...hashset? Can we make out of BasicURI...yes.
        uriIndex : Dictionary<Uri, SubjectNode>; 
    } 

/// Functions relevant to the SubjectGraph data structure.
module SubjectGraph = 

    let emptyGraph () =  
        // TODO: parameterize by catalog type. LOC only for now.
        let topNode = {
            uri = Uri "http://knowledgeincoding.net/classif/00top";
            name = "Top Level"; // TODO: add variant names to subjectNameIndex. OK to keep this as canonical-only?
            subdividedName = ["Top Level"];
            callNumRange = Some {startCN = LCCN.lettersOnlyCN "A"; 
                                 endCN = LCCN.lettersOnlyCN "ZZ"};
            cnString = Some "A-ZZ"; // just as a backup
            broader = new List<SubjectNode>(); // formerly SubjectNode list
            narrower = new List<SubjectNode>(); 
            seeAlso = [];
            books = new List<BookRecord>();
            booksUnder = 0  // to keep a count
        }
        let uriIndex = new Dictionary<_,_>()
        uriIndex.Add(topNode.uri, topNode)
        let cnIndex = new Dictionary<_,_> ();
        cnIndex.Add(topNode.callNumRange.Value, topNode)
        // Don't bother adding top to name indexes.
        { 
            topNode = topNode;
            cnIndex = cnIndex;
            subjectNameIndex = new Dictionary<_,_> ();
            subjectPrefixIndex = NamePrefixIndex.Create ();
            uriIndex = uriIndex
        }

    /// Probably more efficient than maintaining the prefixIndex, but not sufficient.
    /// TODO: move this into a "SubjectSegmentIndex" module.
    let findLongestPrefixSubj graph (splitSubj : string list) = 
        let rec findit slist = 
            match slist with 
                | [] -> []
                | slist -> 
                    let label = SubjectNode.joinSubjectName slist
                    if graph.subjectNameIndex.ContainsKey label then
                        graph.subjectNameIndex.[label]
                    else findit slist.[..List.length slist - 2]
        findit splitSubj

    /// Return the node for the spot in the graph an item should hang on.
    /// Is total; if CN never falls in a range it will go with the given node.
    let findParentByCallNumber graph (cn: LCCN) =
        let rec searchParent (atnode: SubjectNode) = 
            let candidates = 
                (*Seq.filter (mapOr (fun range -> CNRange.contains range cn) false) 
                           atnode.narrower *)
                Seq.filter (fun nd -> if Option.isNone nd.callNumRange then false
                                      else CNRange.contains nd.callNumRange.Value cn) 
                           atnode.narrower
            match List.ofSeq candidates with 
            | head :: rest -> // get most specific range.
                List.fold 
                    (fun nd1 nd2 -> 
                        if CNRange.mostSpecificRange nd1.callNumRange.Value nd2.callNumRange.Value 
                            = nd2.callNumRange.Value
                        then nd2
                        else nd1) 
                    head
                    rest
                |> searchParent
            | [] -> atnode
        searchParent graph.topNode
    
    /// Return the node for a given CN range, trying exact match first,
    ///  else find the most direct parent of that range.
    /// (Will be) used to generate a link to follow for "see also".
    let findCNRange graph (cnRange: LCCNRange) = 
        if graph.cnIndex.ContainsKey cnRange then
            graph.cnIndex.[cnRange]
        elif graph.cnIndex.ContainsKey {cnRange with endCN = cnRange.startCN} then
            graph.cnIndex.[{cnRange with endCN = cnRange.startCN}]
        else
            findParentByCallNumber graph cnRange.startCN

        
    /// Search subject names for a string under a given starting node, 
    /// returning list of only topmost nodes that match.
    let search graph startURI (searchStr : string) = 
        let searchStr = searchStr.ToLower()
        let rec search' fromnode = 
            if fromnode.name.ToLower().Contains searchStr then [fromnode]
            else 
                Seq.collect search' fromnode.narrower
                |>  Seq.toList
        search' graph.uriIndex.[startURI] // need exception handling? here?

    /// Totally awesome, perfect, clear, generic node insertion function.
    /// Dependency injection! an isChild comparison function: CNRange.isSubRange
    /// Will not create a new top node.
    let insertNode (isNarrower : SubjectNode -> SubjectNode -> bool) graph newNode =
        // Find the location of the new node in existing graph, returning parents and children.
        // Assumes node isn't already in the graph.
        let rec insert atnode = 
            // check if it goes between atnode and any of its children.
            let childCandidates = Seq.filter (fun nd -> isNarrower nd newNode) atnode.narrower
            if not (Seq.isEmpty childCandidates)
            then // should I pick the broadest?
                newNode.narrower.AddRange childCandidates
                newNode.broader.Add atnode
                // Oops, I have to build a list and remove later from "atnode"
                // Seq.iter (fun child -> atnode.narrower.Remove child |> ignore) childCandidates
                Seq.iter (fun child -> child.broader.Remove atnode |> ignore) childCandidates
                Seq.iter (fun child -> child.broader.Add newNode |> ignore) childCandidates
                (atnode, List.ofSeq childCandidates)
            else 
                // Try to find nodes it goes below and recurse
                // ...what if multiple could be parents? Let it happen and see!
                match Seq.tryFind (fun nd -> isNarrower newNode nd) atnode.narrower with
                | Some foundParent -> insert foundParent
                // Node doesn't go below any; it must be a sibling of atnode.narrower.
                | None -> 
                    newNode.broader.Add atnode
                    // nothing added to newNode's children.
                    // atnode.narrower.Add newNode // don't do it twice!
                    (atnode, [])
        let (parent, oldchildren) = insert graph.topNode
        // Remove children from grandparent node (couldn't modify atnode in the let rec)
        List.iter (parent.narrower.Remove >> ignore) oldchildren
        // Oops, I forgot to add as a child of the parent!
        parent.narrower.Add newNode
        // Add to the indexes. 
        graph.subjectPrefixIndex.Add newNode // Will this work if it's empty?
        graph.uriIndex.Add(newNode.uri, newNode)
        // If subject has a call number, add that to the index.
        match newNode.callNumRange with 
            | Some cn -> 
                if graph.cnIndex.ContainsKey cn then
                    printfn "[Warning] Overwriting existing subject for CN range %s" (CNRange.toString cn)
                    // graph.cnIndex.[cn] <- newNode :: graph.cnIndex.[cn]
                graph.cnIndex.[cn] <- newNode
            | None -> ()

    // mapSubTree function: apply a function recursively to the subtree starting at a node.

    // Remove all subtrees with zero elements
    let cullGraph graph =
        let mutable numRemoved = 0 // Ideally, would fold this in the result...
        let rec cull' node = 
            // let toRemove = Seq.filter (fun cn -> cn.booksUnder = 0) node.narrower
            // Seq.iter (fun cn -> node.narrower.Remove(cn) |> ignore) toRemove
            // removed <- removed + Seq.length toRemove
            numRemoved <- numRemoved + node.narrower.RemoveAll(fun cn -> cn.booksUnder = 0)
            Seq.iter cull' node.narrower
        cull' graph.topNode
        numRemoved
    
    /// If a subtree has a small number of books, pull them all up into the root
    let collapseGraph graph thresh = 
        // Return a list of all items below a list of nodes.
        let rec harvestItems nodelist = 
            Seq.collect
                (fun nd -> Seq.append nd.books (harvestItems nd.narrower)) 
                nodelist
        let rec collapse' node = 
            if node.booksUnder < thresh then
                node.books.AddRange(harvestItems node.narrower)
                node.narrower.Clear()
            else
                Seq.iter collapse' node.narrower
        collapse' graph.topNode

    /// Shorten long paths by eliminating nodes with one child and no books.
    let contractGraph graph = 
        let mutable removedCount = 0
        let rec contract' node depth = 
            if node.narrower.Count = 1 && node.books.Count = 0
            then
                let child = node.narrower.[0]
                printfn "Removing node (%s) with child (%s) at depth %d" 
                        node.name child.name depth
                for parent in node.broader do 
                    parent.narrower.Remove(node) |> ignore
                    parent.narrower.Add(child)
                child.broader.Remove(node) |> ignore
                child.broader.AddRange(node.broader)
                removedCount <- removedCount + 1
                contract' child (depth+1)
            else
                // Okay to recurse on a copy, because nodes *at this level*
                //  will never be deleted. Right?
                let narrowerCopy = new List<_>(node.narrower) // so iteration won't fail.
                Seq.iter (fun nd -> contract' nd (depth+1)) narrowerCopy
        contract' graph.topNode 0
        printfn "Contract removed %d nodes" removedCount

    /// This step is needed after all nodes are inserted, to find and grab
    /// URI's of the best target node for a cross reference.
    let updateCrossrefs graph = 
        let rec update' atnode = 
            let rec genlist' crlist = 
                match crlist with
                | (desc, Some cnRange, None) :: rest -> 
                    let uri = (findCNRange graph cnRange).uri
                    (desc, Some cnRange, Some uri) :: genlist' rest
                | entry :: rest -> 
                    entry :: genlist' rest
                | [] -> []
            atnode.seeAlso <- genlist' atnode.seeAlso
            Seq.iter update' atnode.narrower
        update' graph.topNode
                   

    /// Part of finalizing a graph for browsing. Add all parent-less nodes to top level.
    /// Possible TODO: Put all such code in a "finalize" method that outputs a new type?
    (* let makeTopLevel graph = 
        for node in graph.uriIndex.Values do
            if node.broader.Count = 0 then
                graph.topLevel.Add(node) *)

(* end module SubjectGraph ************************************************ *)

/// Return true if 1st URI is higher in the graph than the 2nd.
let rec isBroaderSubject (graph: SubjectGraph) (uri1: Uri) (uri2: Uri) = 
    // ? should it accept a node? It should be one-to-one, but...
    let (node1, node2) = graph.uriIndex.[uri1], 
                         graph.uriIndex.[uri2]
    if node2.broader.Count = 0 then
        false
    elif node2.broader.Contains(node1)  then
        true
    else 
        Seq.exists (fun n -> isBroaderSubject graph uri1 n.uri) node2.broader

(******** Stuff below here is old: SPARQL querying functions and CLI. *******)

// Not used by addSubjectByCN (but may be added back for more logic)
let getBroaderTerms (subj : Uri) = 
    let queryString = 
        queryPrefix
        + "SELECT ?broader ?label WHERE {\n"
        +      "<" + subj.ToString() + "> madsrdf:hasBroaderAuthority ?broader .\n"
        + "    ?broader madsrdf:isMemberOfMADSCollection lcsh:collection_LCSH_General .\n"
        + "    ?broader madsrdf:authoritativeLabel ?label .\n"
        + "} LIMIT 10 \n"
        // and general collection?
    Logger.Debug queryString
    let res = sparqlQuery queryString
    [ for bindings in res.results -> (bindings.["broader"], bindings.["label"]) ]
    // List.map (fun (k : Map<string,string>) -> "<" + k.["broader"] + ">") res.results

let getNarrowerTerms (subj : Uri) = 
    let queryString = 
        queryPrefix
        + "SELECT ?narrower WHERE {\n"
        +      "<" + subj.ToString() + "> madsrdf:hasNarrowerAuthority ?narrower .\n"
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
// Way to construct a call number range when there isn't one: take min and
// max of narrower ones (assuming the letters match) *)

let newSubjectUri (graph: SubjectGraph) label callLetters = 
    // TODO: make sure we don't repeat, have a counter in the graph to keep track.
    // call num then five digits? count backwards?
    Uri <| "http://knowledgeincoding.net/classif/" + (callLetters |? "XX") + label

/// Take a subject label and optional call number, and attempt to find its
///   place in the graph.
/// Modifies the graph and returns a node option with the new or found node.
let rec addSubjectLCSH (graph: SubjectGraph) (label: string) (callLetters : string option) =
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
        let subdividedName = SubjectNode.splitSubjectName label
        // Make a URI for the subject, either from LCSH or our own generation scheme.
        let uri = 
            if (not qres.results.IsEmpty) then 
                Uri qres.results.[0].["subj"]
            else 
                // I'd like to have a count of how many weren't found in LCSH...
                Logger.Info <| "Subject label " + label + " not found in LCSH"
                newSubjectUri graph label callLetters
        let cnString = 
            if (not qres.results.IsEmpty) && qres.results.[0].ContainsKey "callNum" then 
                    Some (qres.results.[0].["callNum"])
                else callLetters
        let newNode = { 
            uri = uri;
            name = label; // keep variant names in hash table 
            subdividedName = subdividedName
            cnString = cnString;
            callNumRange = 
                if Option.isSome cnString then
                    Some (CNRange.parse cnString.Value)
                else 
                    None
            broader = new List<SubjectNode>(); // (parents);
            narrower = new List<SubjectNode>(); //(children); 
            seeAlso = [];
            books = new List<BookRecord>();
            booksUnder = 0
        }
        SubjectGraph.insertNode SubjectNode.isNarrower graph newNode // find its place, fill in broader & narrower.
        Some newNode
        //if parents.IsEmpty then None else Some newNode
    
        // Then add this to the broader's narrower list. 
        // Also a query to get variant names; add them to the hash table 

/// Add a book to the graph by call number, returning updated item record if 
/// successful.
let addItemByCallNumber (graph: SubjectGraph) (item: BookRecord) =
    match item.LCCallNum with
    | None -> None
    | Some cn -> 
        let parentNode = SubjectGraph.findParentByCallNumber graph cn
        parentNode.books.Add item
        let rec updateCounts node = 
            node.booksUnder <- node.booksUnder + 1
            Seq.iter updateCounts node.broader
        updateCounts parentNode
        printfn "...added under %s" parentNode.name
        BookRecord.updateSubject item (SubjectNode.toSubjectInfo parentNode) 
        |> Some

/// Add a book's subjects to a graph (and the book too if selected)
/// NOT CURRENTLY USED for CN-based graph.
let addBookSubjects (graph : SubjectGraph) (addBook : bool) (book : BookRecord) =
    // Try to add a subject for each name, get node back 
    let nodes = book.Subjects
                |> List.map (fun subj -> addSubjectLCSH graph subj.name book.LCLetters) 
                |> List.choose id
    Logger.Info <| "Finished adding subjects for book \"" + book.Title + "\""
    // If we're storing books too, add it and update the counts
    if addBook then
        // Add URIs for the found subjects to the book record.
        let updatedBook = 
            (List.map SubjectNode.toSubjectInfo nodes)
            |> List.fold BookRecord.updateSubject book 
        // update the booksUnder count upward.
        let rec updateCounts node = 
            node.booksUnder <- node.booksUnder + 1
            //printf "."
            Seq.iter updateCounts node.broader
        for node in nodes do
            // Only add a book under a node if there's no narrower one.
            if not (List.exists (fun n -> isBroaderSubject graph node.uri n.uri) nodes) then
                printfn "Adding book under node %s" node.name
                node.books.Add(updatedBook)
                updateCounts node
    if nodes.IsEmpty then
        Logger.Alert (sprintf "No subject found for \"%s\"; book not added" book.Title)
        false
    else true

/// Loop to browse a graph. Currently only does a tree walk (no sideways links)
/// To make generic: pass in a "getCommand", "outputSubjectList" and "outputBookList" funs. 
let browseGraphCLI (graph : SubjectGraph) = 
    // Remember our path back to the root. 
    let hist = new Stack<SubjectNode> ()
    (* Might like to print out some level info. *)
    (* Should also be getting the book count underneath and skipping if there's just one 
       (to avoid long single chains) *)
    printfn "*** Top Level Subjects ***" // Shouldn't be here... 
    // letrec is preferable to a while loop and mutable variable? 
    let rec loop (currentNode : SubjectNode) = 
        let currentList = currentNode.narrower
        // When I go up, construct mutable list.. why not make an immutable list?
        List.iteri (fun i node -> 
                        let entryString = 
                            (sprintf "| %d. %s (%d) " i node.name node.booksUnder)
                        printf "%-50s" entryString
                        if i%2 = 1 || entryString.Length > 50 then printf "\n"
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
                    hist.Push currentNode
                    // If the selected node has books, print them. Later, add options to this.
                    if not (Seq.isEmpty (currentList.[i].books)) then
                        printfn "\n**** Books for subject \"%s\" ****" currentList.[i].name
                        for book in currentList.[i].books do
                            printfn "][ %s (%s)" book.Title book.Authors
                            if Option.isSome book.Link then printfn "   %s" book.Link.Value
                        (* currentList.[i].books |> 
                            Seq.iter (fun (book: BookRecord) -> printfn "][ %s" book.Title) *)
                    printfn ""
                    // Print the title before recursing.
                    if currentList.[i].narrower.Count <> 0 then
                        printfn "*** Subheadings of %s ***" currentList.[i].name
                    loop currentList.[i]
                else 
                    printfn "Index out of range"
                    loop currentNode
            | CharOpt c ->
                match c with 
                    | 'q' -> ()
                    | 'u' -> 
                        if hist.Count = 0 then
                            printfn "Already at top level!"
                            loop currentNode
                        else
                            loop <| hist.Pop ()
                    | 't' -> loop graph.topNode
                    | _ -> 
                        printfn "Unrecognized option"
                        loop currentNode
            | Fail -> 
                printfn "Unrecognized input"
                loop currentNode
        (* active pattern to match and interpret? *)
    loop graph.topNode
        
// Maybe I won't need to use the RDF library at all. 
(*
open RDFSharp.Model

let loadSchema filename = 
    // let g = RDFGraph.FromFile (RDFModelEnums.RDFFormats.RdfXml, filename)
    printf "did something."

let hello name =
    printfn "Hello %s" name
    // let rdffunc = new RDFSharp.Model.RDFGraph ()
    printf "made a graph!"
*)

// deserialize. The opens are a hint that maybe this should go elsewhere...
// or in a module.

open System.IO // for file read and write 
open MBrace.FsPickler
// open Newtonsoft.Json.Bson
// open Newtonsoft.Json

(* /// recreate BiserObjectify serialization code
let buildSerializer () = 
    let serinfo = 
        BiserObjectify.Generator.Run
            (typeof<SubjectGraph>, true, "./output/biser/", true, false, null)
    printfn "%A" serinfo
*)

let loadGraph graphFileName = 
    let instream = File.OpenRead graphFileName
    (* let bsonReader = new BsonDataReader(instream)
    let serializer = new JsonSerializer()
    let graph = serializer.Deserialize<SubjectGraph>(bsonReader) *)
    let serializer = FsPickler.CreateBinarySerializer()
    let graph = serializer.Deserialize<SubjectGraph>(instream)
    instream.Close()
    graph

let saveGraph (graph: SubjectGraph) graphFileName = 
    (* let outstream = File.OpenWrite graphFileName
    let bsonWriter = new BsonDataWriter(outstream)
    let serializer = new JsonSerializer()
    let _ = serializer.ReferenceLoopHandling <- ReferenceLoopHandling.Ignore
    serializer.Serialize(bsonWriter, graph)
    bsonWriter.Close()
    outstream.Close() *)
    let outstream = File.OpenWrite graphFileName
    let serializer = FsPickler.CreateBinarySerializer()
    //let graphFormatter = ZeroFormatterSerializer.Serialize(graph)
    graph.subjectPrefixIndex.Clear() // TODO: may have to remove this.
    serializer.Serialize(outstream, graph)
    (* let pickle = serializer.Pickle(graph)
    outstream.Write(serializer.Pickle(graph), 0, pickle.Length) *)
    outstream.Close() 
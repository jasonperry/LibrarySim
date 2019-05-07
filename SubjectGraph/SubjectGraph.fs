/// Constructing and querying a graph of LOC Subject Headings.
module SubjectGraph

open System
open System.Collections.Generic

open Common
open BookTypes
open CallNumber
open SparqlQuery

/// Where to put this?
let rec isStrictPrefix list1 list2 = 
    match (list1, list2) with
        | (_, []) -> false
        | ([], y::ys) -> true
        | (x::xs, y::ys) -> x = y && isStrictPrefix xs ys

type SubjectNode = {
    uri : Uri;
    name : string; // TODO: add variant names to subjectNameIndex. OK to keep this as canonical-only?
    subdividedName : string list;
    callNumRange : LCCNRange option;
    cnString : string option; // just as a backup
    // no explicit refs needed for these, F# uses reference semantics 
    // I thought it was clever that the upwards are immutable and the downwards aren't. 
    broader : List<SubjectNode>; // SubjectNode list;  So sad, had to make it mutable...
    narrower : List<SubjectNode>; // mutable; new parents not added, but children are
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

/// A class with methods for finding subjects by prefix.
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
                Seq.iter (fun (nd : SubjectNode) -> ignore (childList.Remove nd)) 
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
        cnIndex : Dictionary<LCCNRange, SubjectNode list>;  // maybe not unique 
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
            name = "Top Subject"; // TODO: add variant names to subjectNameIndex. OK to keep this as canonical-only?
            subdividedName = ["Top Subject"];
            callNumRange = Some {startCN = LCCN.lettersOnlyCN "A"; 
                                 endCN = LCCN.lettersOnlyCN "ZZ"};
            cnString = Some "A-ZZ"; // just as a backup
            broader = new List<SubjectNode>(); // formerly SubjectNode list
            narrower = new List<SubjectNode>(); 
            books = new List<BookRecord>();
            booksUnder = 0  // to keep a count
        }
        let uriIndex = new Dictionary<_,_>()
        uriIndex.Add(topNode.uri, topNode)
        let cnIndex = new Dictionary<_,_> ();
        cnIndex.Add(topNode.callNumRange.Value, [topNode])
        // Don't bother adding top to name indexes.
        { 
            topNode = topNode;
            cnIndex = cnIndex;
            subjectNameIndex = new Dictionary<_,_> ();
            subjectPrefixIndex = NamePrefixIndex.Create ();
            uriIndex = uriIndex
        }
    /// Probably more efficient than maintaining the prefixIndex, but not sufficient.
    /// TODO: move this into a "SubjectSegmentIndex" module, like with CNIndex.
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
        List.iter (fun child -> parent.narrower.Remove child |> ignore) oldchildren
        // Oops, I forgot to add as a child of the parent!
        parent.narrower.Add newNode
        // Add to the indexes. 
        graph.subjectPrefixIndex.Add newNode // Will this work if it's empty?
        graph.uriIndex.Add(newNode.uri, newNode)
        // If subject has a call number, add that to the index.
        match newNode.callNumRange with 
            | Some cn -> 
                if graph.cnIndex.ContainsKey cn then
                    graph.cnIndex.[cn] <- newNode :: graph.cnIndex.[cn]
                else graph.cnIndex.Add (cn, [newNode])
            | None -> ()

    /// OBSOLETE. Take a completed subject entry and update the graph structure, based on name segments
    let insertNodeBySubjectSegments graph (newNode: SubjectNode) = 
        // TODO? Check if already in index, since this function is doing the work now?
        // Remove links that this node will go between.
        // *debug*
        (* printfn "%A" (Seq.map (fun nd -> nd.name) newNode.broader)
        printfn "%s" newNode.name
        printfn "%A" (Seq.map (fun nd -> nd.name) newNode.narrower)
        printfn "-------------" *)
        // Attempt to find the best parent node(s).
        let parents = 
            // First, look for a shorter version of a complex subject
            let prefixParents = findLongestPrefixSubj graph newNode.subdividedName
            if not (List.isEmpty prefixParents) then
                prefixParents
            (* elif callLetters.IsSome && graph.cnIndex.ContainsKey callLetters.Value then
                // TODO: find "narrowest call range above"...
                graph.cnIndex.[callLetters.Value] *)
            // old code to use LCSH broader for parents if no call number subject found
            (* elif (not qres.results.IsEmpty) then
                let broaderSubjects = getBroaderTerms uri
                // ISSUE: these might be broader than our top-level. 
                // ISSUE: Need to check/update call number.
                List.map (fun br -> addSubjectByCN graph (snd br) callLetters) broaderSubjects 
                    |> List.choose id  // filters out empties.
                // still recursively add? Yes, but it should stop now!!
                //  what if it has a call number?  *)
            else []  // No more warning...parents can be found later
                // or still might want to return None for now, to see how I'm doing.
        // Get list of node's children. If there are no single-extension 
        // subjects, take them all and later they'll get "wedged between"
        let children = 
            // TODO: correct is to only add extensions with "nothing between"
            graph.subjectPrefixIndex.FindChildren newNode.subdividedName
            (*match graph.subjectPrefixIndex.SingleExtensions subdividedName with
                | [] -> graph.subjectPrefixIndex.AllExtensions subdividedName
                | extns -> extns *)
        // Go ahead and add the parents and children
        newNode.broader.AddRange(parents)
        newNode.narrower.AddRange(children)
        // Loop to remove links this node "goes between".
        for parent in parents do
            let removeFromParentNarrower = new List<SubjectNode>()
            for child in parent.narrower do
                // printfn "comparing %s *to*\n     %s" child.name newNode.name
                if SubjectNode.isNarrower child newNode then
                    // printfn "re-parenting child node %s" child.name
                    // newNode.narrower.Add child  // should already be there
                    removeFromParentNarrower.Add child
                    child.broader.Remove parent |> ignore
                    // child.broader.Add newNode // done below
            Seq.iter (fun nd -> parent.narrower.Remove nd |> ignore) removeFromParentNarrower
            // Is this enough? Do I have to think upward too?
        // Add "backlinks"
        for parent in newNode.broader do
            parent.narrower.Add newNode
        for child in newNode.narrower do 
            child.broader.Add newNode
        // Add key and value to subject name index. TODO: add variants.
        if not (graph.subjectNameIndex.ContainsKey newNode.name) then
            graph.subjectNameIndex.Add (newNode.name, [newNode])
        else
            // It's OK just to append because we won't call this on a node
            // that's already in the graph.
            graph.subjectNameIndex.[newNode.name] <- 
                (newNode :: graph.subjectNameIndex.[newNode.name])
        // 3. Add to the subject prefix index.
        graph.subjectPrefixIndex.Add newNode
        // 3. Add the new subject URI to that index.
        graph.uriIndex.Add(newNode.uri, newNode)
        // 4. If subject has a call number, add that to the index.
        // NOT APPROPRIATE if it's just letters, but doing it anyway. They'll have long lists.
        // Temp change 12/24: pulling out just letters from parsed CNRange.
        match newNode.callNumRange with 
            | Some cn -> 
                if graph.cnIndex.ContainsKey cn then
                    graph.cnIndex.[cn] <- newNode :: graph.cnIndex.[cn]
                else graph.cnIndex.Add (cn, [newNode])
            | None -> ()
    /// Part of finalizing a graph for browsing. Add all parent-less nodes to top level.
    /// Possible TODO: Put all such code in a "finalize" method that outputs a new type?
    (* let makeTopLevel graph = 
        for node in graph.uriIndex.Values do
            if node.broader.Count = 0 then
                graph.topLevel.Add(node) *)

/// Return true if list1 is a strict prefix of list2 (not equal).

(* ***************************************************************** *)

/// Return true if 1st URI is higher in the graph than the 2nd.
let rec isBroaderThan (graph: SubjectGraph) (uri1: Uri) (uri2: Uri) = 
    // ? should it accept a node? It should be one-to-one, but...
    let (node1, node2) = graph.uriIndex.[uri1], 
                         graph.uriIndex.[uri2]
    if node2.broader.Count = 0 then
        false
    elif node2.broader.Contains(node1)  then
        true
    else 
        Seq.exists (fun n -> isBroaderThan graph uri1 n.uri) node2.broader

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
            books = new List<BookRecord>();
            booksUnder = 0
        }
        SubjectGraph.insertNode SubjectNode.isNarrower graph newNode // find its place, fill in broader & narrower.
        Some newNode
        //if parents.IsEmpty then None else Some newNode
    
        // Then add this to the broader's narrower list. 
        // Also a query to get variant names; add them to the hash table 
    
/// Add a book's subjects to a graph (and the book too if selected)
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
            (List.map (fun (nd: SubjectNode) -> 
                {name = nd.name; cnRange = nd.callNumRange; uri = Some nd.uri}) nodes)
            |> List.fold BookRecord.updateSubject book 
        // update the booksUnder count upward.
        let rec updateCounts node = 
            node.booksUnder <- node.booksUnder + 1
            printf "."
            Seq.iter updateCounts node.broader
        for node in nodes do
            // Only add a book under a node if there's no narrower one.
            if not (List.exists (fun n -> isBroaderThan graph node.uri n.uri) nodes) then
                printfn "Adding book under node %s" node.name
                node.books.Add(updatedBook)
                updateCounts node
    if nodes.IsEmpty then
        Logger.Alert (sprintf "No subject found for \"%s\"; book not added" book.Title)
        false
    else true

/// Loop to browse a graph. Currently only does a tree walk (no sideways links)
/// To make generic: pass in a "getCommand", "outputSubjectList" and "outputBookList" funs. 
let browseGraph (graph : SubjectGraph) = 
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
                    if not (currentList.[i].narrower.Count = 0) then
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

// deserialize. The opens are a hint that maybe this should go elsewhere.

open System.IO // for file read and write 
open MBrace.FsPickler
let loadGraph graphFileName = 
    //let booksFormatter = BinaryFormatter()
    let serializer = FsPickler.CreateBinarySerializer()
    let stream = new FileStream(graphFileName, FileMode.Open)
    let graph = serializer.Deserialize<SubjectGraph>(stream)
    stream.Close()
    graph

let saveGraph (graph: SubjectGraph) graphFileName = 
    let binarySerializer = FsPickler.CreateBinarySerializer()
    //let graphFormatter = ZeroFormatterSerializer.Serialize(graph)
    graph.subjectPrefixIndex.Clear() // TODO: may have to remove this.
    let pickle = binarySerializer.Pickle(graph)
    let outfile = File.OpenWrite(graphFileName)
    //stream.Write(ZeroFormatterSerializer.Serialize(graph), 0, 0)
    outfile.Write(binarySerializer.Pickle(graph), 0, pickle.Length)
    // graphFormatter.Serialize(stream, graph)
    outfile.Close()
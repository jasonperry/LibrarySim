// Top-level SubjectGraph Web API

open Suave
open Suave.Filters
open Suave.Operators
open Suave.Utils.Collections
//open Suave.Sockets
open Suave.Writers
open Newtonsoft.Json
open System.Web // HttpUtility.HtmlEncode

open Common
open BookTypes
open CallNumber
open SubjectGraph
open MarcXmlToBooks
open System

// TODO: put these in a configuration file, or get them from the app instance.
let listenIPs = ["127.0.0.1"] //; "192.168.0.13"]
let appPort = 8080

/// Self-contained info about one graph node, to be directly JSONized 
///   and sent to the browser.
/// ...should it contain a pointer to the books? or back to the node itself?
type SubjectsResult = {
  thisSubject : SubjectInfo;
  broader : SubjectInfo list;
  narrower : SubjectInfo list;
  cnRange : string;
  seeAlso: CrossrefInfo option
}
module SubjectsResult = 
  let ofNode (node : SubjectNode) = {
    thisSubject = {
        uri = Some node.uri; 
        cnRange = node.callNumRange; 
        name = node.name;
        itemsUnder = node.booksUnder
    };
    broader = node.broader 
      |> Seq.map SubjectNode.toSubjectInfo
      |> List.ofSeq;
      // Q: Is there a way to cast this to not convert the whole list? I've tried...
    narrower = node.narrower 
      |> Seq.map SubjectNode.toSubjectInfo
      |> List.ofSeq
      |> List.sortWith (fun (si1: SubjectInfo) si2 -> 
                            CNRange.compare si1.cnRange si2.cnRange)
    cnRange = node.cnString |? ""
    seeAlso = node.seeAlso
  }
  let nodeListToInfoList nodes = 
      List.map 
          (fun nd -> {
               uri = Some nd.uri;
               cnRange = nd.callNumRange;
               name = nd.name;
               itemsUnder = nd.booksUnder
           })
          nodes

  /// make a clickable link to a node's URI.
  let makeURILink appUrl (uri : System.Uri) text = 
      "<a href=\"" + appUrl + "/browse?uri=" 
      + uri.ToString() + "\">" +  HttpUtility.HtmlEncode(text) 
      + "</a>"

  let subjectInfoToHtml appUrl (si : SubjectInfo) = 
      // TODO: find a way to get the app's own URL...from the config?
      "<td>" + (mapOr CNRange.toString "[NO CN]" si.cnRange) + "</td>"
      + "<td>" + makeURILink appUrl si.uri.Value 
                             (si.name + " (" + string si.itemsUnder + ")")
      + "</td>"

  let crossrefInfoToHtml appUrl (cr : CrossrefInfo) = 
    cr.desc + ":<br/><bl>" 
    + (cr.refs 
       |> List.map 
          // Fails on graph with unresolved cross-ref
          (fun (range, desc, uriOpt) -> 
              "<li>" + 
              match uriOpt with
              | Some uri -> 
                  makeURILink appUrl uri ((CNRange.toString range) + " " + desc)
              | None -> (CNRange.toString range) + " " + desc
              + "</li>"
          )
       |> String.concat "<br/>")
    + "</bl>"

  let toHtml appUrl (sr : SubjectsResult) = 
      (if List.isEmpty sr.broader then ""
       else
          "<table><tr><td>Up: </td>" 
          + (String.concat "</td><td>" (List.map (subjectInfoToHtml appUrl) sr.broader))
          + "</td></tr></table>")
      + "<h2>" + sr.cnRange + " " + HttpUtility.HtmlEncode(sr.thisSubject.name) + "</h2>"  
      //+ "<p>Call number range: " + sr.cnRange + "<br />"
      + "<p>Items under this heading: " + (string sr.thisSubject.itemsUnder) + "</p>"
      +  match sr.seeAlso with 
         | Some sa -> "<p> <b>Cross references:</b> " + crossrefInfoToHtml appUrl sa + "</p>"
         | None -> ""
      + "</p><table><tr>"
      + String.concat "</tr><tr>" (List.map (subjectInfoToHtml appUrl) sr.narrower)
      + "</tr><table>"

  let infoListToHtml appUrl (infolist : SubjectInfo list) =
      "<p>Found " + string (infolist.Length) + " results.</p>"
      + "<table><tr>"
      + String.concat "</tr><tr>" (List.map (subjectInfoToHtml appUrl) infolist)
      + "</tr></table>"

// end module SubjectsResult

// TODO: monadize the error handling.  -> WebResult string
// The ^^ is the "request combinator"
let getSubjectResult g uri = 
      // TODO: error handling
      SubjectsResult.ofNode g.uriIndex.[uri]

let getSubjectSearchResult (g : SubjectGraph) q = 
    // printf "Doing search..."
    match (q ^^ "searchstr") with
    | Choice1Of2 searchStr -> 
        let startUri = 
            match (q ^^ "fromtop") with
            | Choice1Of2 "false" -> 
                // printf "Got false choice option!";
                match (q ^^ "uri") with
                | Choice1Of2 uri -> System.Uri uri
                | _ -> g.topNode.uri
            | _ -> g.topNode.uri
        SubjectGraph.search g startUri searchStr
        |> SubjectsResult.nodeListToInfoList
    | _ -> []

/// Transmission type of all books under a SubjectNode.
type BooksResult = {
  thisSubject : SubjectInfo; // list? Yes!
  // broader : SubjectInfo list; // Don't need, can just move up from the subject.
  // can view all books under but it gives you chunks!
  books : BookRecord list
}
module BooksResult = 

  let ofNode (node : SubjectNode) = {
    thisSubject = SubjectNode.toSubjectInfo node
    // Sorting assumes there's a call number.
    books = Seq.sortBy (fun (br : BookRecord) -> br.LCCallNum.Value) node.books
            |> List.ofSeq
  }

  let toHtml (bres : BooksResult) = 
    let bookfmt (br : BookRecord) = 
        "<td>" + (mapOr LCCN.toString "" br.LCCallNum) + "</td>"
        + "<td><b>" + HttpUtility.HtmlEncode(br.Title) + "</b></td>"
        + "<td>" + HttpUtility.HtmlEncode(br.Authors) + "</td>"
        + "</tr><tr><td></td>"
        + match br.Link with 
          | Some link -> "<td><a href=\"" + link + "\">" + link + "</a></td>"
          | None -> "<td>(no link)</td>"
        + "</tr>"
    "<div class=\"booklisting\"><table><tr>"
    + (String.concat "</tr><tr>" (List.map bookfmt bres.books))
    + "</tr></table></div>"

// end module BooksResult

/// Return a BookResult object for a URI query.
let getBookResult (g: SubjectGraph) q = 
  Option.ofChoice (q ^^ "uri") |? "Unrecognized variable" 
  |> fun uriStr -> 
         if uriStr = "top" then // Do I not even use this, just URL 00top
           {
               // FIXME: The cnRange setting is a hack, should it be better?
               thisSubject = {
                 uri = None; 
                 cnRange = Some (CNRange.parse "A-ZZ"); 
                 name = "Top Level";
                 itemsUnder = g.topNode.booksUnder
               };
               books = []
           }
         else 
             BooksResult.ofNode g.uriIndex.[System.Uri uriStr]

/// Outputs the header for the SubjectGraph browsing web app.
let pageHeader (r: HttpRequest) = 
    let uriStr = 
        match (r.queryParam "uri") with
        | Choice1Of2 uri -> uri
        | _ -> "SHOULDN'T HAPPEN"
    let atNode = 
        match (r.queryParam "searchstr") with
        | Choice1Of2 sstr -> false
        | _ -> true
    "<html><head><title>SubjectGraph Browser</title><head>" 
    + "<body><table><tr><td width=60%><h1>SubjectGraph</h1></td>"
    + "<td width=40%>"
    + "<form method=GET action=searchsubj>"
    + "<input type=TEXT name=searchstr>Search</input>"
    + "<input type=SUBMIT name=submit value=Go><br/>"
    + if atNode then (
        "<input type=RADIO name=fromtop value=true checked=yes> From top level"
        + "<input type=RADIO name=fromtop value=false> From current node"
        + "<input type=HIDDEN name=uri value=" + uriStr + ">" )
      else "<input type=HIDDEN name=fromtop value=true>"
    + "</form></td></tr>"
    // + "<tr><td>" + locationStr + "</td></tr>"
    + "</table><hr>"

/// Look for "uri" variable in query, validate and run the next function if succeeds.
let withQueryUri query f = 
    match Option.ofChoice (query ^^ "uri") with
    | Some uriStr -> // TODO: parse System.Uri here and give error if bad.
        try
            let uri = System.Uri uriStr
            Successful.OK (f uri)
        with 
            | :? System.UriFormatException -> 
              RequestErrors.bad_request ("Malformed query URI."B)
    | None -> 
          RequestErrors.bad_request ("Unknown request variable."B)

/// Suave dispatcher for the SubjectGraph web app.
let dispatch g =
  choose 
    [ GET >=> choose
        [ // Need to setMimeType "text/html; charset=utf-8"
          path "/subject" 
          >=> setMimeType "text/json; charset=utf-8"
          >=> request (fun r -> 
                withQueryUri r.query (fun uri ->
                    getSubjectResult g uri
                    |> JsonConvert.SerializeObject) )
          path "/books" 
          >=> setMimeType "text/json; charset=utf-8"
          >=> request (fun r -> Successful.OK 
                                  (getBookResult g r.query
                                   |> JsonConvert.SerializeObject))
          path "/browse" // JSON client will get subject and books in ajaxy way?
          >=> setMimeType "text/html; charset=utf-8"
          >=> request (fun r ->  
                match r.query with
                // If no query, redirect to the top node.
                | [] -> 
                  Redirection.see_other 
                      (r.url.GetLeftPart(System.UriPartial.Authority) 
                       + "/browse?uri=" 
                       + string (g.topNode.uri))
                | _  -> 
                  withQueryUri r.query (fun uri ->
                      (pageHeader r 
                       + SubjectsResult.toHtml 
                         (r.url.GetLeftPart(System.UriPartial.Authority))
                         (getSubjectResult g uri)
                       + "<hr>"
                       + BooksResult.toHtml (getBookResult g r.query)
                       + "</body></html>")) )
          path "/searchsubj"
          >=> setMimeType "text/html; charset=utf-8"
          >=> request (fun r -> 
                Successful.OK
                  (pageHeader r
                    + "<p><a href=\"" + r.url.GetLeftPart(System.UriPartial.Authority) 
                    + "/browse?uri=" 
                    + string (g.topNode.uri) + "\">Back to top</a></p>"
                    + "<h2>Search result for: " 
                    // Maybe I should deal with all the variables here, so the
                    // getSubjectSearchResult function can be cleaner.
                    + (Option.ofChoice (r.queryParam "searchstr") |? ":empty:")
                    // TODO: show the name of the node it's under. Need to return more search info?
                    //+ " under: " + (Option.ofChoice (r.queryParam "uri") |? "(top)") + "</h2>"
                    + SubjectsResult.infoListToHtml 
                        (r.url.GetLeftPart(System.UriPartial.Authority)) 
                        (getSubjectSearchResult g r.query)
                    + "</body></html>"))
          path "/"
          >=> setMimeType "text/html; charset=utf-8"
          >=> request (fun r ->
                Successful.OK
                  ("<html><body>Welcome to the SubjectGraph Web Application.<p />"
                   + "Entry points:"
                   + "<ul><li><a href=\"/browse\">/browse</a> : user interface</li>"
                   + "<li>/subject?uri=... : JSON for subject URI</li>"
                   + "<li>/books?uri=... : JSON for books under a subject URI</li>"
                   + "</ul></body></html>"))
      ]
      (* POST >=> choose
        [ path "/hello" >=> Successful.OK "Hello POST"
          path "/goodbye" >=> Successful.OK "Good bye POST" ] *) 
    ]

[<EntryPoint>]
let main argv =
  match argv.[0] with 
  | "buildTopLevel" -> 
      BuildTopLevel.writeTopLevelGraph ()
      0
  | "buildLCClassGraph" -> 
      BuildLCClassGraph.buildGraph argv.[1] "output/ClassGraph.sgb"
      0
  | "addBooksToClassGraph" -> 
      let graph = loadGraph argv.[1]
      addBooksToClassGraph graph argv.[2]
      if argv.Length > 3 then 
        saveGraph graph argv.[3]
      else
        saveGraph graph "output/BooksAndClassGraph.sgb"
      0
  | "cullGraph" ->
        let graph = loadGraph argv.[1]
        let removed = SubjectGraph.cullGraph graph
        printfn "Removed %d nodes from graph, saving..." removed
        saveGraph graph "output/CulledGraph.sgb"
        0
  | "collapseGraph" ->
        let graph = loadGraph argv.[1]
        let outGraphName = "output/CollapsedGraph.sgb"
        printfn "Loaded graph %s" argv.[1]
        SubjectGraph.collapseGraph graph (int argv.[2])
        printfn "Saving culled graph %s" outGraphName
        saveGraph graph outGraphName
        0
  | "cullapseGraph" ->
      let graph = loadGraph argv.[1]
      let outGraphName = "output/CullapsedGraph.sgb"
      printfn "Loaded graph %s" argv.[1]
      SubjectGraph.collapseGraph graph (int argv.[2])
      let removed = SubjectGraph.cullGraph graph
      printfn "Removed %d nodes; saving collapsed/culled graph %s" removed outGraphName
      saveGraph graph outGraphName
      0
  | "contractGraph" ->
      let graph = loadGraph argv.[1]
      let outGraphName = "output/ContractedGraph.sgb"
      SubjectGraph.contractGraph graph
      printfn "Saving contracted graph %s" outGraphName
      saveGraph graph outGraphName
      0
  | "minimizeGraph" -> // do all the things.
      let graph = loadGraph argv.[1]
      let outGraphName = "output/MinimizedGraph.sgb"
      printfn "Loaded graph %s" argv.[1]
      SubjectGraph.collapseGraph graph (int argv.[2])
      let removed = SubjectGraph.cullGraph graph
      SubjectGraph.contractGraph graph // TODO : removed 2, add them together.
      printfn "Removed %d nodes; saving minimized graph %s" removed outGraphName
      saveGraph graph outGraphName
      0
  | "buildGutenBooks" ->
      MarcXmlToBooks.processBooks argv.[1]
      0
  | "buildGutenGraph" ->
      // any way to detect if records.brb is up to date? Not bothering yet!
      BuildFromBooks.buildGraph argv.[1]
      0
  | "browse" ->
      printfn "Loading graph %s" argv.[1]
      let graph = loadGraph argv.[1]
      browseGraphCLI graph
      0
  | "serve" -> 
      let theGraph = loadGraph argv.[1]
      printfn "Loaded subject graph"
      startWebServer 
        { defaultConfig with 
            bindings = List.map (fun ip -> HttpBinding.createSimple HTTP ip appPort) 
                                listenIPs 
        }
        (dispatch theGraph)
      0
  | "update" ->
      // SubjectGraph.buildSerializer()
      0
  | _ -> 
      printfn "Unknown argument: %s" argv.[0]
      1
      
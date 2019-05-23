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

// TODO: put these in a configuration file, or get them from the app instance.
let listenIPs = ["127.0.0.1"] //; "192.168.0.13"]
let appPort = 8080
let appRoot = "http://" + listenIPs.[0] + ":" + (string appPort) + "/"

/// Immutable SubjectNode info returned by the API, to be directly JSONized 
///   and sent to the browser.
/// ...should it contain a pointer to the books? or back to the node itself?
type SubjectsResult = {
  thisSubject : SubjectInfo;
  broader : SubjectInfo list;
  narrower : SubjectInfo list;
  cnRange : string;
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

  let formatSubjectInfo (si : SubjectInfo) = 
      // TODO: find a way to get the app's own URL...from the config?
      "<td>" + (mapOr CNRange.toString "[NO CN]" si.cnRange) + "</td>"
      + "<td><a href=\"" + appRoot + "browse?uri=" 
      + si.uri.Value.ToString() + "\">" +  HttpUtility.HtmlEncode(si.name) 
      + " (" + string si.itemsUnder + ") </a></td>"

  let toHtml (sr : SubjectsResult) = 
      (if List.isEmpty sr.broader then ""
       else
          "<table><tr><td>Up: </td>" 
          + (String.concat "</td><td>" (List.map formatSubjectInfo sr.broader))
          + "</td></tr></table>")
      + "<h2>" + HttpUtility.HtmlEncode(sr.thisSubject.name) + "</h2>"  
      + "<p>Call number range: " + sr.cnRange + "<br />"
      + "Entries under this heading: " + (string sr.thisSubject.itemsUnder) + "</p>"
      + "<table><tr>"
      + String.concat "</tr><tr>" (List.map formatSubjectInfo sr.narrower)
      + "</tr><table>"

  let infoListToHtml (infolist : SubjectInfo list) =
      "<p>Found " + string (infolist.Length) + " results.</p>"
      + "<table><tr>"
      + String.concat "</tr><tr>" (List.map formatSubjectInfo infolist)
      + "</tr></table>"

  /// Construct a result object corresponding to the top level. Now we have a top node
  (* let topLevel (g : SubjectGraph) = 
    let topSubjects = List.ofSeq (g.topLevel)
    {
      thisSubject = {uri = None; name = "Top Level"};
      broader = [];
      narrower = topSubjects
        |> List.map (fun nd -> {uri = Some nd.uri; name = nd.name});
      booksUnder = List.sumBy (fun (nd : SubjectNode) -> nd.booksUnder) topSubjects
      cnRange = "A-Z"
    } *)
// end module SubjectsResult

// TODO: monadize the error handling.  -> WebResult string
// The ^^ is the "request combinator"
let getSubjectResult g q = 
   Option.ofChoice (q ^^ "uri") |? "Unrecognized variable" 
  |> fun uriStr -> 
    (*if uriStr = "top" then 
      SubjectsResult.topLevel g
    else *)
      // TODO: error handling
      SubjectsResult.ofNode g.uriIndex.[System.Uri uriStr]

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
    books = List.ofSeq node.books
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

let dispatch g =
  choose 
    [ GET >=> choose
        [ // Need to setMimeType "text/html; charset=utf-8"
          path "/subject" 
          >=> request (fun r -> Successful.OK 
                                  (getSubjectResult g r.query
                                   |> JsonConvert.SerializeObject))
          path "/books" 
          >=> request (fun r -> Successful.OK 
                                  (getBookResult g r.query
                                   |> JsonConvert.SerializeObject))
          path "/browse" // JSON client will get subject and books in ajaxy way?
          >=> setMimeType "text/html; charset=utf-8"
          >=> request (fun r -> Successful.OK 
                                  (pageHeader r 
                                   + SubjectsResult.toHtml (getSubjectResult g r.query)
                                   + "<hr>"
                                   + BooksResult.toHtml (getBookResult g r.query)
                                   + "</body></html>")) 
          path "/searchsubj"
          >=> setMimeType "text/html; charset=utf-8"
          >=> request (fun r -> Successful.OK
                                  (pageHeader r
                                   + "<p><a href=\"" + appRoot + "browse?uri=" 
                                   + string (g.topNode.uri) + "\">Back to top</a></p>"
                                   + "<h2>Search result for: " 
                                   // Maybe I should deal with all the variables here, so the
                                   // getSubjectSearchResult function can be cleaner.
                                   + (Option.ofChoice (r.queryParam "searchstr") |? ":empty:")
                                   // TODO: show the name of the node it's under. Need to return more search info?
                                   //+ " under: " + (Option.ofChoice (r.queryParam "uri") |? "(top)") + "</h2>"
                                   + SubjectsResult.infoListToHtml (getSubjectSearchResult g r.query)
                                   + "</body></html>"))
        ]
      (* POST >=> choose
        [ path "/hello" >=> Successful.OK "Hello POST"
          path "/goodbye" >=> Successful.OK "Good bye POST" ] *) ]

// end module BooksResult

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
      browseGraph graph
      0
  | "serve" -> 
      let theGraph = loadGraph argv.[1]
      printfn "Loaded subject graph"
      startWebServer 
        { defaultConfig with 
            bindings = List.map (fun ip -> HttpBinding.createSimple HTTP ip appPort) 
                                listenIPs 
        }
        (dispatch theGraph) //(Successful.OK "Hello, Suave!")
      0
  | _ -> 
      printfn "Unknown argument: %s" argv.[0]
      1
      
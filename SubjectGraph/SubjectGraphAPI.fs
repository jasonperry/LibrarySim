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
open SubjectGraph

// TODO: put these in a configuration file.
let listenIPs = ["127.0.0.1"] //; "192.168.0.13"]

/// Immutable SubjectNode info returned by the API, to be directly JSONized 
///   and sent to the browser.
/// ...should it contain a pointer to the books? or back to the node itself?
type SubjectsResult = {
  thisSubject : SubjectInfo;
  broader : SubjectInfo list;
  narrower : SubjectInfo list;
  cnRange : string;
  booksUnder : int
}
module SubjectsResult = 
  let ofNode (node : SubjectNode) = {
    thisSubject = {uri = Some node.uri; name = node.name};
    broader = node.broader 
      |> Seq.map (fun nd -> {uri = Some nd.uri; name = nd.name})
      |> List.ofSeq;
      // Q: Is there a way to cast this to not convert the whole list? I've tried...
    narrower = node.narrower 
      |> Seq.map (fun nd -> {uri = Some nd.uri; name = nd.name})
      |> List.ofSeq;
    cnRange = node.cnString |? ""
    booksUnder = node.booksUnder
  }
  let toHtml (sr : SubjectsResult) = 
    let makeSubjectInfoLink (si : SubjectInfo) = 
      // TODO: find a way to get the app's own URL...from the config?
      "<a href=\"http://127.0.0.1:8080/browse?uri=" 
      + si.uri.Value.ToString() + "\">" +  HttpUtility.HtmlEncode(si.name) + "</a>"
    
    (if List.isEmpty sr.broader then 
         "<p>Up: <a href=\"http://127.0.0.1:8080/browse?uri=http://knowledgeincoding.net/classif/00top\">Top level</a></p>" 
     else
         "Up: " + (String.concat " " (List.map makeSubjectInfoLink sr.broader)))
    + "<h1>" + HttpUtility.HtmlEncode(sr.thisSubject.name) + "</h1>"  
    + "<p>Call number range: " + sr.cnRange + "<br />"
    + "Entries under this heading: " + (string sr.booksUnder) + "</p>"
    + String.concat "<br />" (List.map makeSubjectInfoLink sr.narrower)

  /// Construct a result object corresponding to the top level. No longer needed?
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

/// Transmission type of all books under a SubjectNode.
type BooksResult = {
  thisSubject : SubjectInfo; // list? Yes!
  // broader : SubjectInfo list; // Don't need, can just move up from the subject.
  // can view all books under but it gives you chunks!
  books : BookRecord list
}
module BooksResult = 
  let ofNode (node : SubjectNode) = {
    thisSubject = {uri = Some node.uri; name = node.name};
    books = List.ofSeq node.books
  }
  let toHtml (bres : BooksResult) = 
    let bookfmt (br : BookRecord) = 
        "<b>" + HttpUtility.HtmlEncode(br.Title) + "</b><br />"
        + HttpUtility.HtmlEncode(br.Authors) + "<br />"
        + match br.Link with 
          | Some link -> "<a href=\"" + link + "\">" + link + "</a>"
          | None -> "(no link)"
    "<div class=\"booklisting\">"
    + (String.concat "" (List.map (fun br -> "<p>" + bookfmt br + "</p>") bres.books))
    + "</div>"

let getBookResult (g: SubjectGraph) q = 
  Option.ofChoice (q ^^ "uri") |? "Unrecognized variable" 
  |> fun uriStr -> 
         if uriStr = "top" then
             {
               thisSubject = {uri = None; name = "Top Level"};
               books = []
             }
         else 
             BooksResult.ofNode g.uriIndex.[System.Uri uriStr]

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
                                  ("<html><body>"
                                   + SubjectsResult.toHtml (getSubjectResult g r.query)
                                   + "<hr>"
                                   + BooksResult.toHtml (getBookResult g r.query)
                                   + "</body></html>")) ]
      POST >=> choose
        [ path "/hello" >=> Successful.OK "Hello POST"
          path "/goodbye" >=> Successful.OK "Good bye POST" ] ]

[<EntryPoint>]
let main argv =
  match argv.[0] with 
  | "buildLCClassGraph" -> 
      BuildLCClassGraph.buildGraph argv.[1]
      0
  | "buildTopLevel" -> 
      BuildTopLevel.writeTopLevelGraph ()
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
            bindings = List.map (fun ip -> HttpBinding.createSimple HTTP ip 8080) 
                                listenIPs 
        }
        (dispatch theGraph) //(Successful.OK "Hello, Suave!")
      0
  | _ -> 
      printfn "Unknown argument: %s" argv.[0]
      1
      
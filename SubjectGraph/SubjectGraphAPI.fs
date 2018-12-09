// Top-level SubjectGraph Web API

open Suave
open Suave.Filters
open Suave.Operators
open Suave.Utils.Collections
//open Suave.Sockets
open Newtonsoft.Json

open BookTypes
open SubjectGraph

/// Immutable SubjectNode info returned by the API.
type SubjectsResult = {
  thisSubject : SubjectInfo;
  broader : SubjectInfo list;
  narrower : SubjectInfo list;
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
  }
  let toHtml (sr : SubjectsResult) = 
    let makeSubjectInfoLink (si : SubjectInfo) = 
      // TODO: find a way to get the app's own URL.
      "<p><a href=\"http://127.0.0.1:8080/browse?uri=" 
      + si.uri.Value.ToString() + "\">" + si.name + "</a></p>"
    "<html><body>" + String.concat "" (List.map makeSubjectInfoLink sr.broader)
      + "<h1>" + sr.thisSubject.name + "</h1>"  
      + String.concat "" (List.map makeSubjectInfoLink sr.narrower) + "</body></html>"

  /// Construct a result object corresponding to the top level.
  let topLevel (g : SubjectGraph) = {
    thisSubject = {uri = None; name = "Top Level"};
    broader = [];
    narrower = List.ofSeq (g.topLevel)
      |> List.map (fun nd -> {uri = Some nd.uri; name = nd.name});
  }

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

// TODO: monadize the error handling.  -> WebResult string
// The ^^ is the "request combinator"
let getSubjectResult g q = 
  defaultArg (Option.ofChoice (q ^^ "uri")) "Unrecognized variable" 
  |> fun uriStr -> 
    if uriStr = "top" then 
      SubjectsResult.topLevel g
    else 
      SubjectsResult.ofNode g.uriIndex.[System.Uri uriStr]

let getBookResult (g: SubjectGraph) q = 
  defaultArg (Option.ofChoice (q ^^ "uri")) "Unrecognized variable" 
  |> fun uriStr -> BooksResult.ofNode g.uriIndex.[System.Uri uriStr]

let dispatch g =
  choose 
    [ GET >=> choose
        [ // Need to setMimeType "text/html; charset=utf-8"
          path "/subject" >=> request (fun r -> Successful.OK 
                                                  (getSubjectResult g r.query
                                                   |> JsonConvert.SerializeObject))
          path "/books" >=> request (fun r -> Successful.OK (getBookResult g r.query
                                                             |> JsonConvert.SerializeObject))
          path "/browse" >=> request (fun r -> Successful.OK (getSubjectResult g r.query
                                                              |> SubjectsResult.toHtml)) ]
      POST >=> choose
        [ path "/hello" >=> Successful.OK "Hello POST"
          path "/goodbye" >=> Successful.OK "Good bye POST" ] ]

[<EntryPoint>]
let main argv =
  match argv.[0] with 
    | "buildClassGraph" -> 
      BuildClassGraph.buildGraph argv.[1]
      0
    | "buildTopLevel" -> 
      BuildTopLevel.buildGraph ()
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
      let theGraph = loadGraph "output/graph.sgb"
      printfn "Loaded subject graph"
      startWebServer defaultConfig (dispatch theGraph) //(Successful.OK "Hello, Suave!")
      0
    | _ -> 
      printfn "Unknown argument"
      1
      
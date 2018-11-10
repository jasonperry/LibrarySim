// Top-level SubjectGraph Web API

open Suave
open Suave.Filters
open Suave.Operators
open Suave.Utils.Collections
//open Suave.Sockets
open Newtonsoft.Json

open BookTypes
open SubjectGraph

type SubjectsResult = {
  thisSubject : SubjectInfo;
  broader : SubjectInfo list;
  narrower : SubjectInfo list;
}
module SubjectsResult = 
  let ofNode (node : SubjectNode) = {
    thisSubject = {uri = Some node.uri; name = node.name};
    broader = node.broader 
      |> List.map (fun nd -> {uri = Some nd.uri; name = nd.name});
      // Q: Is there a way to cast this to not convert the whole list? I've tried...
    narrower = List.ofSeq node.narrower 
      |> List.map (fun nd -> {uri = Some nd.uri; name = nd.name});
  }
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
  |> JsonConvert.SerializeObject

let getBookResult (g: SubjectGraph) q = 
  defaultArg (Option.ofChoice (q ^^ "uri")) "Unrecognized variable" 
  |> fun uriStr -> BooksResult.ofNode g.uriIndex.[System.Uri uriStr]
  |> JsonConvert.SerializeObject

let dispatch g =
  choose 
    [ GET >=> choose
        [ 
          path "/subject" >=> request (fun r -> Successful.OK (getSubjectResult g r.query))
          path "/books" >=> request (fun r -> Successful.OK (getBookResult g r.query)) ]
      POST >=> choose
        [ path "/hello" >=> Successful.OK "Hello POST"
          path "/goodbye" >=> Successful.OK "Good bye POST" ] ]

[<EntryPoint>]
let main _ =
  let theGraph = loadGraph "output/graph.sgb"
  printfn "Loaded subject graph"
  startWebServer defaultConfig (dispatch theGraph) //(Successful.OK "Hello, Suave!")
  0
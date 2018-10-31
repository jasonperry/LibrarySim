open SubjectGraph
open Suave
open Suave.Filters
open Suave.Operators
open Suave.Utils.Collections
//open Suave.Sockets
open Newtonsoft.Json

type SubjectsResult = {
  thisSubjectUri : string;
  subjectName : string;
  broader : string list;
  narrower : string list;
}
module SubjectsResult = 
  let ofNode (node : SubjectNode) = {
    thisSubjectUri = node.uri.Value;
    subjectName = node.name;
    broader = List.map (fun node -> node.uri.Value) node.broader;
    narrower = List.map (fun node -> node.uri.Value) (List.ofSeq node.narrower);
  }

type BooksResult = {
  thisSubjectUri : string;
  subjectName : string;
  broader : string list;
  // can view all books under but it gives you chunks!
  books : BookRecord.BookRecord list
}

// TODO: monadize the error handling.  -> WebResult string
let getSubjectResult g q = 
  defaultArg (Option.ofChoice (q ^^ "uri")) "Unrecognized variable" 
  |> fun uri -> SubjectsResult.ofNode g.uriIndex.[Uri uri]
  |> JsonConvert.SerializeObject

let dispatch g =
  choose 
    [ GET >=> choose
        [ path "/hello" >=> Successful.OK "Hello GET"
          path "/subject" >=> request (fun r -> Successful.OK (getSubjectResult g r.query))]
      POST >=> choose
        [ path "/hello" >=> Successful.OK "Hello POST"
          path "/goodbye" >=> Successful.OK "Good bye POST" ] ]

[<EntryPoint>]
let main _ =
  let theGraph = loadGraph "output/graph.sgb"
  printfn "Loaded subject graph"
  startWebServer defaultConfig (dispatch theGraph) //(Successful.OK "Hello, Suave!")
  0
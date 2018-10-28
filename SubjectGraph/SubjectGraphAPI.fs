open SubjectGraph
open Suave
open Suave.Filters
open Suave.Operators
open Suave.Utils.Collections
//open Suave.Sockets
//open Newtonsoft.Json

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


// Should understand how the runtime loading will work.
let theGraph = loadGraph "output/graph.sgb"
printfn "Loaded subject graph"

let nodeToJSON node = 
    "{" + "}"
(* let browseAtSubject uri =
    // pull out URIs of broader and narrower (plus books).
    let node = theGraph.uriIndex.[uri]
    nodeToJSON node *)

let sendSubject q = 
  defaultArg (Option.ofChoice (q ^^ "uri")) "World" |> sprintf "Chose subject uri %s"

let dispatch =
  choose 
    [ GET >=> choose
        [ path "/hello" >=> Successful.OK "Hello GET"
          path "/goodbye" >=> Successful.OK "Good bye GET" 
          path "/subject" >=> request (fun r -> Successful.OK (sendSubject r.query))]
      POST >=> choose
        [ path "/hello" >=> Successful.OK "Hello POST"
          path "/goodbye" >=> Successful.OK "Good bye POST" ] ]

[<EntryPoint>]
let main _ =
  startWebServer defaultConfig dispatch //(Successful.OK "Hello, Suave!")
  0
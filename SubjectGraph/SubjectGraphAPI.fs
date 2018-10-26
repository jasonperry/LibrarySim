module SubjectGraphAPI

open SubjectGraph
open Suave.Web
open Suave.Successful

[<EntryPoint>]
let main argv =
  startWebServer defaultConfig (OK "Hello, Suave!")
  0

// Should understand how the runtime loading will work.
(* let theGraph = loadGraph "output/graph.sgb"

let nodeToJSON node = 
    "{" + "}"
let browseAtSubject uri =
    // pull out URIs of broader and narrower (plus books).
    let node = theGraph.uriIndex.[uri]
    nodeToJSON node
*)
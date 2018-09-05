#r "System.Xml.Linq.dll"
#load "CallNumber.fs"
#load "BookRecord.fs"
#load "Library.fs"
open SubjectGraph

// just change this and try.
//let subjs = ["Baseball"; "Basketball"; "Volleyball"]
let subjs = ["Presidents--United States--Inaugural addresses"]

List.map (addSubject theGraph) subjs |> ignore

browseGraph theGraph

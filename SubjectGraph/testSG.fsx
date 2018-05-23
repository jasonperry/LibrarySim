#r "System.Xml.Linq.dll"
#load "CallNumber.fs"
#load "BookRecord.fs"
#load "Library.fs"
open SubjectGraph

let subjs = ["Baseball"; "Basketball"; "Volleyball"]

List.map (addSubject theGraph) subjs |> ignore

browseGraph theGraph

let _ = 42
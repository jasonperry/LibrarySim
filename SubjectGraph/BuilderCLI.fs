// Graph building console main

open Common
open BookTypes
open CallNumber
open SubjectGraph
open MarcXmlToBooks
open Mods
open System

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
      let outGraphName = 
          if argv.Length > 3 then 
              argv.[3] 
          else "output/BooksAndClassGraph.sgb"
      addBooksToClassGraph graph argv.[2]
      saveGraph graph outGraphName
      printfn "Saved graph with books as %s" outGraphName
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
      SubjectGraph.collapseGraph graph (int argv.[2]) // threshold
      let removed = SubjectGraph.cullGraph graph
      SubjectGraph.contractGraph graph // TODO : removed 2, add them together.
      printfn "Removed %d nodes; saving minimized graph %s" removed outGraphName
      saveGraph graph outGraphName
      0
  | "processBooks" ->
      MarcXmlToBooks.processBooks argv.[1]
      0
  | "printBooks" -> 
      // TODO: have sort-by command-line options -scl, -scn, etc.
      Console.OutputEncoding <- System.Text.Encoding.UTF8
      let books = BookTypes.loadBooks argv.[1]
      let sortedBooks = BookTypes.sortBooksByCallLetters books
      for book in sortedBooks do 
          printfn "%s : %s" (BookRecord.getLCCNString book) book.Title
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
  // Placeholder for graph or books update, to be added.
  | "update" ->
      // SubjectGraph.buildSerializer()
      0
  | "mods" ->
      Mods.test1 ()  
      0    
  | _ -> 
      printfn "Unknown argument: %s" argv.[0]
      1
      
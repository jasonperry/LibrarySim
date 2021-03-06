// Graph building console main

open Common
open BookTypes
open CallNumber
open SubjectGraph
open MarcXmlToBooks
open Mods
open System

let OUTDIR = __SOURCE_DIRECTORY__ + @"/output/"

[<EntryPoint>]
let main argv =
  match argv.[0] with 
  | "buildTopLevel" -> 
      BuildTopLevel.writeTopLevelGraph ()
      0
  | "buildLCClassGraph" -> 
      BuildLCClassGraph.buildGraph argv.[1] (OUTDIR + "ClassGraph.sgb")
      0
  | "linkBooksToGraph" -> 
      let graph = loadGraph argv.[1]
      let booksDBFile = argv.[2]
      let outGraphName = 
          if argv.Length > 3 then argv.[3] 
          else (OUTDIR + "BooksAndClassGraph.sgb")
      // Everything uses seq's, so just chain it together.
      // Uhh, but it's too slow. Maybe separating is faster?
      // Or still a transaction issue?
      let updatedBooks = 
        booksDBFile
        |> loadBooks
        |> linkBooksToGraph graph
      saveBookUpdates booksDBFile updatedBooks
      saveGraph graph outGraphName  // it still modifies the graph...
      printfn "Saved graph with books as %s" outGraphName
      0
  | "cullGraph" ->
      let graph = loadGraph argv.[1]
      let outGraphName = OUTDIR + "CulledGraph.sgb"
      let removed = SubjectGraph.cullGraph graph
      printfn "Removed %d nodes from graph, saving %s" removed outGraphName
      saveGraph graph outGraphName
      0
  | "collapseGraph" ->  // <graphFile> <bookdb> <cutoff>
      let graph = loadGraph argv.[1]
      let booksDB = new BooksDB(argv.[2])
      let outGraphName = OUTDIR + "CollapsedGraph.sgb"
      printfn "Loaded graph %s" argv.[1]
      SubjectGraph.collapseGraph graph booksDB (int argv.[3])
      printfn "Saving culled graph %s" outGraphName
      saveGraph graph outGraphName
      0
  (*| "cullapseGraph" ->  // <graphFile> <bookDB>
      let graph = loadGraph argv.[1]
      let outGraphName = "output/CullapsedGraph.sgb"
      printfn "Loaded graph %s" argv.[1]
      let booksDB = new BooksDB(argv.[2])
      SubjectGraph.collapseGraph graph booksDB (int argv.[3])
      let removed = SubjectGraph.cullGraph graph
      printfn "Removed %d nodes; saving collapsed/culled graph %s" removed outGraphName
      saveGraph graph outGraphName
      0 *)
  | "contractGraph" ->  // <graphFile> <bookDB>
      let graph = loadGraph argv.[1]
      let booksDB = new BooksDB(argv.[2])
      booksDB.DbConn.Open()  // should I make a destructor?
      let outGraphName = OUTDIR + "ContractedGraph.sgb"
      SubjectGraph.contractGraph graph booksDB
      booksDB.DbConn.Close()
      printfn "Saving contracted graph %s" outGraphName
      saveGraph graph outGraphName
      0
  | "minimizeGraph" -> // do all the things.
      let graph = loadGraph argv.[1]
      let outGraphName = OUTDIR + "MinimizedGraph.sgb"
      printfn "Loaded graph %s" argv.[1]
      // UPDATE: Need to cull before contract
      let removed = SubjectGraph.cullGraph graph
      let booksDB = new BooksDB(argv.[2])
      booksDB.DbConn.Open()
      SubjectGraph.collapseGraph graph booksDB (int argv.[3]) // threshold
      SubjectGraph.contractGraph graph booksDB // TODO : removed 2, add them together.
      booksDB.DbConn.Close()
      printfn "Removed %d nodes; saving minimized graph %s" removed outGraphName
      saveGraph graph outGraphName
      0
  | "addBooksToDB" ->
      let marcXmlFile = if argv.[1] = "-A" then argv.[2] else argv.[1]
      let append = argv.[1] = "-A"
      let bookRecords = MarcXmlToBooks.processBooks marcXmlFile
      BookTypes.saveBooks bookRecords (OUTDIR + "books.sqlite") append
      0
  | "printBooks" -> 
      // TODO: have sort-by command-line options -scl, -scn, etc.
      Console.OutputEncoding <- System.Text.Encoding.UTF8
      let books = BookTypes.loadBooks argv.[1]
      let sortedBooks = BookTypes.sortBooksByCallLetters books
      for book in sortedBooks do 
          printfn "%s : %s" book.LCCNString book.Title
      0
  (* | "buildGutenGraph" ->
      // any way to detect if records.brb is up to date? Not bothering yet!
      BuildFromBooks.buildGraph argv.[1]
      0 *)
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
      
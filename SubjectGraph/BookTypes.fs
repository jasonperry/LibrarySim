/// Global types related to book records (including subject info).
module BookTypes

open System // Uri
open System.IO
open System.Runtime.Serialization.Formatters.Binary // TODO: maybe replace with FsPickler.
open System.Collections.Generic
open System.Data.SQLite

open Common
open Logger
open CallNumber

/// Primary type for information about a single catalog item.
type BookRecord = {
    Title: string
    Authors: string
    // LCCallNum : LCCN option  // parsed version (maybe should change)
    LCCallNumFields: struct {| a: string; b: string |}  // can be empty string
    Subjects: List<string * System.Uri option> // now mutable, formerly SubjectInfo list
    Year: int option
    Uri : Uri
    Links: string list
} with
    member this.LCCNString = 
        if this.LCCallNumFields.a = "" then ""
        else this.LCCallNumFields.a + this.LCCallNumFields.b

    /// Maybe don't have this at all, just try parsing when we add to the graph.
    /// Then we can ditch the CallNum dependency. OTOH, having a parsed CN with the
    /// book could be useful for other applications as well.
    member this.LCCallNum =
        try
            Some (LCCN.parse (this.LCCallNumFields.a + this.LCCallNumFields.b))
        with 
            | CallNumberException errorstr -> 
                Logger.Error "Error parsing item %s call number: %s" this.Title errorstr
                None

    /// Replace the subject information for a book with a new record.
    member this.UpdateSubject (subjName, subjLink : System.Uri) = 
        match List.tryFindIndex  // Remove any existing subject with matching name OR Uri.
            (fun (name, uriOpt) -> 
                name = subjName ||
                match uriOpt with 
                    | Some uri -> uri = subjLink
                    | None -> false)
            (List.ofSeq this.Subjects) with 
            | Some matchIndex -> this.Subjects.RemoveAt matchIndex
            | None -> ()
        printfn "Adding subject link %A" subjLink
        this.Subjects.Add (subjName, Some subjLink)


/// TODO: determine if still needed.
/// sort a list of books by call number string only (for Gutenberg).
let sortBooksByCallLetters (books: seq<BookRecord>) = 
    books
    |> Seq.sortBy (fun br -> br.LCCNString)


type BooksDB (dbFileName) = 
    // Initialize the connection object. TODO: detect existing DB
    // Ideas before bed: public "getReadConn", "createConn" members.
    //  ....no the object should maintain those, but they should not necessarily
    //   be initialized.
    let dbConn = 
        let connectionString = sprintf "Data Source=%s;Version=3;" dbFileName  
        let connection = new SQLiteConnection(connectionString)
        connection

    // Not private so the browser program can open it once for all.
    member this.DbConn with get() = dbConn

    /// Open database and create the tables.
    member private this.InitDB () = 
        let bookTableCmd = 
            "CREATE TABLE books (" +
            "title TEXT NOT NULL, " +
            "bookUri TEXT PRIMARY KEY, " +
            "authors TEXT, " +
            "year INTEGER, " + 
            "LCCallNumber TEXT, " + 
            "LCItemNumber TEXT )" 
        let subjectsTableCmd = 
            "CREATE TABLE subjects (" +
            "subjectName TEXT, " +
            "subjectUri TEXT, " +
            "bookUri TEXT, " +
            "FOREIGN KEY(bookUri) REFERENCES books(bookUri) )"
        let linksTableCmd = 
            "CREATE TABLE bookLinks (" +
            "link TEXT, " +
            "bookUri TEXT, " +
            "FOREIGN KEY (bookUri) REFERENCES books(bookUri) )"
        SQLiteConnection.CreateFile(dbFileName)
        this.DbConn.Open()
        use ccmd1 = new SQLiteCommand(bookTableCmd, this.DbConn)
        let c1res = ccmd1.ExecuteNonQuery()
        use ccmd2 = new SQLiteCommand(subjectsTableCmd, this.DbConn)
        let c2res = ccmd2.ExecuteNonQuery()
        use ccmd3 = new SQLiteCommand(linksTableCmd, this.DbConn)
        let c3res = ccmd3.ExecuteNonQuery()
        printfn "Initialized connection, %d, %d, %d rows affected" c1res c2res c3res
        // Close is done at the end of AddBooks.
        
    member this.AddBooks (books: seq<BookRecord>) = 
        this.InitDB() 
        let insertBookCommand = 
            "INSERT INTO books (title, bookUri, authors, year, LCCallNumber, LCItemNumber) " +
            "VALUES (@title, @bookUri, @authors, @year, @LCCallNumber, @LCItemNumber)"
        let insertSubjectsCommand = 
            "INSERT INTO subjects (subjectName, subjectUri, bookUri) " +
            "VALUES (@subjectName, @subjectUri, @bookUri)"
        let insertLinksCommand =
            "INSERT INTO bookLinks (link, bookUri) VALUES (@link, @bookUri)"
        // Wrapping the inserts in BEGIN/END makes it one transaction - much faster.
        use command = new SQLiteCommand("BEGIN", this.DbConn)
        command.ExecuteNonQuery() |> ignore
        let resLength = 
            books
            |> Seq.map (fun (br: BookRecord) -> 
                use command = new SQLiteCommand(insertBookCommand, this.DbConn)
                command.Parameters.AddWithValue("@title", br.Title) |> ignore
                command.Parameters.AddWithValue("@bookUri", br.Uri) |> ignore
                command.Parameters.AddWithValue("@authors", br.Authors) |> ignore
                command.Parameters.AddWithValue("@year", 
                    Option.defaultValue 0 br.Year) |> ignore 
                command.Parameters.AddWithValue("@LCCallNumber", 
                    br.LCCallNumFields.a) |> ignore
                command.Parameters.AddWithValue("@LCItemNumber", 
                    br.LCCallNumFields.b) |> ignore 
                command.ExecuteNonQuery() |> ignore
                // Insert into subject table.
                br.Subjects 
                |> Seq.iter (fun (subjName, subjLink) -> 
                    use command = new SQLiteCommand(insertSubjectsCommand, this.DbConn)
                    command.Parameters.AddWithValue("@subjectName", subjName) |> ignore
                    command.Parameters.AddWithValue("@subjectUri", 
                        Option.defaultValue null subjLink) |> ignore
                    command.Parameters.AddWithValue("@bookUri", br.Uri) |> ignore
                    command.ExecuteNonQuery() |> ignore )
                // Insert into links table.
                br.Links 
                |> Seq.iter (fun linkString -> 
                    use command = new SQLiteCommand(insertLinksCommand, this.DbConn)
                    command.Parameters.AddWithValue("@link", linkString) |> ignore
                    command.Parameters.AddWithValue("@bookUri", br.Uri) |> ignore
                    command.ExecuteNonQuery() |> ignore ) )
            |> Seq.length // a count of units. Ha! 
        use command = new SQLiteCommand("END", this.DbConn)
        command.ExecuteNonQuery() |> ignore
        this.DbConn.Close()
        resLength
    
    member this.UpdateSubjects (books: seq<BookRecord>) = 
        this.DbConn.Open()
        let allBooks = new System.Collections.Generic.List<_>(books)
        use startCmd = new SQLiteCommand("BEGIN", this.DbConn)
        startCmd.ExecuteNonQuery() |> ignore
        (* let updateSubjectsCommand = 
            "UPDATE subjects SET subjectName=@subjectName, subjectUri=@subjectUri " +
            "WHERE bookUri=@bookUri" *)
        let updateSubjectsCommand = 
            "INSERT INTO subjects (subjectName, subjectUri, bookUri) " +
            "VALUES (@subjectName, @subjectUri, @bookUri)"
        printfn "--> Updating subjects for %d books..." allBooks.Count
        let resLength = 
            allBooks
            |> Seq.map (fun (br: BookRecord) -> 
                // printfn "Book now has %d subjects" br.Subjects.Count
                br.Subjects
                |> Seq.iter (fun (subjName, subjLink) -> 
                    match subjLink with  // Only need to add if there's a link.
                    | Some subjLinkValue ->
                        use command = new SQLiteCommand(updateSubjectsCommand, this.DbConn)
                        command.Parameters.AddWithValue("@subjectName", subjName) |> ignore
                        command.Parameters.AddWithValue("@subjectUri", subjLinkValue) |> ignore
                        command.Parameters.AddWithValue("@bookUri", br.Uri) |> ignore
                        // printfn "Constructed update command: %s" (command.ToString())
                        command.ExecuteNonQuery() |> ignore 
                    | None -> () )
            ) |> Seq.length
        use command = new SQLiteCommand("END", this.DbConn)
        command.ExecuteNonQuery() |> ignore
        this.DbConn.Close()
        resLength

    member this.LoadAllBooks () = 
        this.DbConn.Open()
        (* use startCmd = new SQLiteCommand("BEGIN", this.DbConn)
        startCmd.ExecuteNonQuery() |> ignore *)
        let transaction = this.DbConn.BeginTransaction()
        let selectQueryStr = 
            "SELECT title, bookUri, authors, year, LCCallNumber, LCItemNumber FROM books"
        use selectCommand = new SQLiteCommand(selectQueryStr, this.DbConn)
        let booksReader = selectCommand.ExecuteReader()
        printfn "SELECT books command executed"
        let res = seq {
            while booksReader.Read() do
                // printfn ("Read a book result")
                let bookUriStr = booksReader.["bookUri"].ToString()
                let subjectsQueryStr = 
                    sprintf
                        "SELECT subjectName, subjectUri FROM subjects WHERE bookUri = '%s'" 
                        bookUriStr
                use subjectsQueryCmd = new SQLiteCommand(subjectsQueryStr, this.DbConn)
                let subjectsReader = subjectsQueryCmd.ExecuteReader()
                let subjects = new List<string * System.Uri option>()
                while subjectsReader.Read() do 
                    if not (subjectsReader.IsDBNull(1)) then 
                        subjects.Add (subjectsReader.["subjectName"].ToString(), 
                            Some (System.Uri(subjectsReader.["subjectUri"].ToString())))
                    else 
                        subjects.Add (subjectsReader.["subjectName"].ToString(), None)
                let linksQueryStr = 
                    sprintf
                        "SELECT link FROM bookLinks WHERE bookUri = '%s'" bookUriStr
                use linksQueryCmd = new SQLiteCommand(linksQueryStr, this.DbConn)
                let linksReader = linksQueryCmd.ExecuteReader()
                // let subjects = new List<string * System.Uri option>()
                let links = 
                    seq {
                        while linksReader.Read() do 
                            yield linksReader.["link"].ToString()
                    } 
                    |> List.ofSeq
                // printfn "About to parse %s" (booksReader.["LCCallNumber"].ToString())
                yield { 
                    Title = booksReader.["title"].ToString()
                    Authors = booksReader.["authors"].ToString()
                    Uri = System.Uri(bookUriStr)
                    LCCallNumFields = 
                        struct {|
                                  a = booksReader.["LCCallNumber"].ToString()
                                  b = booksReader.["LCItemNumber"].ToString()
                        |}
                    Subjects = subjects
                    Year = match int (booksReader.["year"].ToString()) with 
                           | 0 -> None 
                           | yr -> Some yr
                    Links = links
                }
        }
        (* use endCmd = new SQLiteCommand("END", this.DbConn)
        endCmd.ExecuteNonQuery() |> ignore *)
        transaction.Commit()
        // this.DbConn.Close() // this was causing the trouble
        res

    member this.BooksForSubject (subjectUri: System.Uri) = 
        // could it be faster if I prep the query and store it with the subjectNode?
        // ... a thunk!
        let bookQueryStr = 
            "SELECT title, books.bookUri, authors, year, LCCallNumber, LCItemNumber " +
            "FROM books INNER JOIN subjects ON books.bookUri = subjects.bookUri " +
            "WHERE subjects.subjectUri = @subjectUri "
        use bookQuery = new SQLiteCommand(bookQueryStr, this.DbConn)
        bookQuery.Parameters.AddWithValue("@subjectUri", 
            subjectUri.ToString()) |> ignore
        // fun () -> 
        let transaction = this.DbConn.BeginTransaction()
        let booksReader = bookQuery.ExecuteReader()
        let result = seq {
            while booksReader.Read() do
                // printfn ("Read a book result")
                let bookUriStr = booksReader.["bookUri"].ToString()
                let linksQueryStr = 
                    sprintf
                        "SELECT link FROM bookLinks WHERE bookUri = '%s'" bookUriStr
                use linksQueryCmd = new SQLiteCommand(linksQueryStr, this.DbConn)
                let linksReader = linksQueryCmd.ExecuteReader()
                // let subjects = new List<string * System.Uri option>()
                let links = 
                    seq {
                        while linksReader.Read() do 
                            yield linksReader.["link"].ToString()
                    } 
                    |> List.ofSeq
                yield { 
                    Title = booksReader.["title"].ToString()
                    Authors = booksReader.["authors"].ToString()
                    Uri = System.Uri(bookUriStr)
                    LCCallNumFields = 
                        struct {|
                                  a = booksReader.["LCCallNumber"].ToString()
                                  b = booksReader.["LCItemNumber"].ToString()
                        |}
                    Subjects = new List<_>()
                    Year = match int (booksReader.["year"].ToString()) with 
                           | 0 -> None 
                           | yr -> Some yr
                    Links = links
                }
        }
        transaction.Commit()
        result

    interface System.IDisposable with
        member this.Dispose () =
            this.DbConn.Close()
// end class BooksDB

/// Detect the type of book persistence and load the database.
let loadBooks (filename: string) = 
    if filename.EndsWith "sqlite" then 
        let bookDB = new BooksDB(filename)  // got an error with "use"
        bookDB.LoadAllBooks()
    else
        let formatter = BinaryFormatter()
        let stream = new FileStream(filename, FileMode.Open)
        let bookSeq = formatter.Deserialize stream :?> seq<BookRecord>
        stream.Close()
        bookSeq

let saveBooks (books: seq<BookRecord>) (filename: string) = 
    if filename.EndsWith "sqlite" then 
        use bookDB = new BooksDB(filename)
        let numAdded = bookDB.AddBooks books
        printfn "Wrote %d books to database %s" numAdded filename
    else
        let formatter = BinaryFormatter()
        let stream = new FileStream(filename, FileMode.Create)
        formatter.Serialize(stream, books)
        stream.Close()

let saveBookUpdates (filename: string) (books: seq<BookRecord>) =
    use bookDB = new BooksDB(filename)
    let numUpdated = bookDB.UpdateSubjects books
    printfn "Updated %d books in database %s" numUpdated filename
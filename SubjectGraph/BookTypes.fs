/// Global types related to book records (including subject info).
module BookTypes

open System // Uri
open System.IO
open System.Runtime.Serialization.Formatters.Binary // TODO: maybe replace with FsPickler.
open System.Collections.Generic
open System.Data.SQLite

open Common
open CallNumber

/// Primary type for information about a single catalog item.
type BookRecord = {
    Title: string
    Authors: string
    LCCallNum : LCCN option
    LCLetters: string option
    Subjects: List<string * System.Uri option> // now mutable, formerly SubjectInfo list
    Year: int option
    Uri : Uri
    Links: string list
}

module BookRecord = 
    /// Replace the subject information for a book with a new record.
    let updateSubject record (subjName, subjLink : System.Uri) = 
        match List.tryFindIndex 
            (fun (name, uriOpt) -> 
                name = subjName ||
                match uriOpt with 
                    | Some uri -> uri = subjLink
                    | None -> false)
            (List.ofSeq record.Subjects) with 
            | Some matchIndex -> record.Subjects.RemoveAt matchIndex
            | None -> ()
        record.Subjects.Add (subjName, Some subjLink)

    let getLCCNString record = 
        match record.LCCallNum with 
        | Some cn -> LCCN.toString cn
        | None -> record.LCLetters |? "--none--"


// should sortBooks be here or somewhere else?
/// sort a list of books by call letters only (for Gutenberg).
let sortBooksByCallLetters (books: seq<BookRecord>) = 
    books
    |> Seq.sortWith (fun br1 br2 -> 
        match (br1.LCLetters, br2.LCLetters) with 
        | (None, None) -> 0
        | (None, Some _) -> 1 // Nones go at end
        | (Some _, None) -> -1
        | (Some cl1, Some cl2) -> String.Compare(cl1, cl2)
    )

let loadBooks (filename: string) = 
    let formatter = BinaryFormatter()
    let stream = new FileStream(filename, FileMode.Open)
    let bookSeq = formatter.Deserialize stream :?> seq<BookRecord>
    stream.Close()
    bookSeq

let saveBooks (books: seq<BookRecord>) filename = 
    let formatter = BinaryFormatter()
    let stream = new FileStream(filename, FileMode.Create)
    formatter.Serialize(stream, books)
    stream.Close()

type BooksDB (dbFileName) = 
    let bookTableCmd = 
        "CREATE TABLE books (" +
        "title TEXT NOT NULL, " +
        "bookUri TEXT PRIMARY KEY, " +
        "authors TEXT, " +
        "year INTEGER, " + 
        "LCCallNumber TEXT, " + 
        "LCLetters TEXT )" 
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
    // Initialize the connection object. TODO: detect existing DB
    let dbConn = 
        SQLiteConnection.CreateFile(dbFileName)
        let connectionString = sprintf "Data Source=%s;Version=3;" dbFileName  
        let connection = new SQLiteConnection(connectionString)
        connection.Open()
        use ccmd1 = new SQLiteCommand(bookTableCmd, connection)
        let c1res = ccmd1.ExecuteNonQuery()
        use ccmd2 = new SQLiteCommand(subjectsTableCmd, connection)
        let c2res = ccmd2.ExecuteNonQuery()
        use ccmd3 = new SQLiteCommand(linksTableCmd, connection)
        let c3res = ccmd3.ExecuteNonQuery()
        printfn "Initialized connection, %d, %d, %d rows affected" c1res c2res c3res
        connection

    member this.DbConn with get() = dbConn

    member this.AddBooks (bookList: BookRecord list) = 
        let insertBookCmd = 
            "INSERT INTO books (title, bookUri, authors, year, LCCallNumber, LCLetters) " +
            "VALUES (@title, @bookUri, @authors, @year, @LCCallNumber, @LCLetters)"
        let insertSubjectsCommand = 
            "INSERT INTO subjects (subjectName, subjectUri, bookUri) " +
            "VALUES (@subjectName, @subjectUri, @bookUri)"
        let insertLinksCommand =
            "INSERT INTO bookLinks (link, bookUri) VALUES (@link, @bookUri)"
        use command = new SQLiteCommand("BEGIN", this.DbConn)
        command.ExecuteNonQuery() |> ignore
        let resLength = 
            bookList
            |> List.map (fun (br: BookRecord) -> 
                let command = new SQLiteCommand(insertBookCmd, this.DbConn)
                command.Parameters.AddWithValue("@title", br.Title) |> ignore
                command.Parameters.AddWithValue("@bookUri", br.Uri) |> ignore
                command.Parameters.AddWithValue("@authors", br.Authors) |> ignore
                command.Parameters.AddWithValue("@year", 
                    Option.defaultValue 0 br.Year) |> ignore 
                command.Parameters.AddWithValue("@LCCallNumber", 
                    mapOr LCCN.toString null br.LCCallNum) |> ignore 
                command.Parameters.AddWithValue("@LCLetters", 
                    Option.defaultValue null br.LCLetters) |> ignore  // and here
                let res = command.ExecuteNonQuery()
                command.Dispose()
                // printfn "Inserted %d book row(s)" res
                List.ofSeq br.Subjects 
                |> List.iter (fun (subjName, subjLink) -> 
                    let command = new SQLiteCommand(insertSubjectsCommand, this.DbConn)
                    command.Parameters.AddWithValue("@subjectName", subjName) |> ignore
                    command.Parameters.AddWithValue("@subjectUri", 
                        Option.defaultValue null subjLink) |> ignore
                    command.Parameters.AddWithValue("@bookUri", br.Uri) |> ignore
                    let res = command.ExecuteNonQuery()
                    command.Dispose() )
                    // printfn "Inserted %d subject row(s)" res) 
                List.ofSeq br.Links 
                |> List.iter (fun linkString -> 
                    let command = new SQLiteCommand(insertLinksCommand, this.DbConn)
                    command.Parameters.AddWithValue("@link", linkString) |> ignore
                    command.Parameters.AddWithValue("@bookUri", br.Uri) |> ignore
                    let res = command.ExecuteNonQuery()
                    command.Dispose() ) )
                    // printfn "Inserted %d link row(s)" res)
            |> List.length // a count of units. Ha!
        let command = new SQLiteCommand("END", this.DbConn)
        command.ExecuteNonQuery() |> ignore
        resLength
    
    interface System.IDisposable with
        member this.Dispose () =
            this.DbConn.Close()
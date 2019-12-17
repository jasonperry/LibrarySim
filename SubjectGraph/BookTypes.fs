/// Global types related to book records (including subject info).
module BookTypes

open System // Uri
open System.IO
open System.Runtime.Serialization.Formatters.Binary // TODO: maybe replace with FsPickler.
open System.Collections.Generic

open Common
open CallNumber

/// Primary type for information about a single catalog item.
type BookRecord = {
    Title: string
    Authors: string
    LCCallNum : LCCN option
    LCLetters: string option
    Subjects: List<System.Uri> // now mutable, formerly SubjectInfo list
    Year: int option
    Uri : Uri
    Links: string list
}

module BookRecord = 
    /// Replace the subject information for a book with a new record.
    let updateSubject record (subjLink : System.Uri) = 
        if not (record.Subjects.Contains(subjLink)) then
            record.Subjects.Add(subjLink)
        (* let rec update uriList = 
            match uriList with
                | [] -> [subjLink]
                | info :: rest -> 
                    // Find the matching subject by name.
                    if subjLink.name = info.name then
                        subjLink :: rest
                    else 
                        info :: (update rest)
        { record with Subjects = update record.Subjects } *)
    let getLCCNString record = 
        match record.LCCallNum with 
        | Some cn -> LCCN.toString cn
        | None -> record.LCLetters |? "--none--"

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
// should sortBooks be here or somewhere else?
open System.Collections.Generic

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
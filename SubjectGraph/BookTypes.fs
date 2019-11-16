/// Global types related to book records (including subject info).
module BookTypes

open System // Uri
open System.IO
open System.Runtime.Serialization.Formatters.Binary // TODO: maybe replace with FsPickler.

open Common
open CallNumber

/// A record about a subject, not dependent on SubjectGraph.
[<Struct>] // Does "struct" make it more efficient? Measure!
type SubjectInfo = {
  uri : System.Uri option;
  cnRange : LCCNRange option;
  name : string;
  itemsUnder : int;
}

/// Primary type for information about a single catalog item.
type BookRecord = {
    Title: string
    Authors: string
    LCCallNum : LCCN option
    LCLetters: string option
    Subjects: SubjectInfo list
    Year: int option
    Uri : Uri
    Link: string option
}

module BookRecord = 
    /// Replace the subject information for a book with a new record.
    let updateSubject record (sinfo : SubjectInfo) = 
        let rec update infolist = 
            match infolist with
                | [] -> [sinfo]
                | info :: rest -> 
                    // Find the matching subject by name.
                    if sinfo.name = info.name then
                        sinfo :: rest
                    else 
                        info :: (update rest)
        { record with Subjects = update record.Subjects }
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
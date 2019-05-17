/// Global types related to book records (including subject info).
module BookTypes

open System // Uri
open CallNumber

/// A record about a subject, not dependent on SubjectGraph.
[<Struct>] // Does "struct" make it more efficient? Measure!
type SubjectInfo = {
  uri : System.Uri option;
  cnRange : LCCNRange option;
  name : string
}

/// Primary type for information about a single catalog item.
type BookRecord = {
    Title: string
    Authors: string
    LCCallNum : LCCN option
    LCLetters: string option
    Subjects: SubjectInfo list
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
                    if sinfo.name = info.name then
                        sinfo :: rest
                    else 
                        info :: (update rest)
        { record with Subjects = update record.Subjects }
 
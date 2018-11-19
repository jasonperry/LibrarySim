/// Global types related to book records (including subject info).
module BookTypes

open CallNumber

[<Struct>] // Will this make it more efficient? Measure!
type SubjectInfo = {
  uri : System.Uri option;
  name : string
}

type BookRecord = {
    Title: string
    Authors: string
    LCCallNum : LCCN option
    LCLetters: string option
    Subjects: SubjectInfo list
    Link: string option
}

module BookRecord = 
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
 
module BookRecord

open CallNumber

type BookRecord = {
    Title: string
    Authors: string
    LCCall : LCCN option
    Subjects: string list
}

module BookRecord

open CallNumber

type Book = {
    Title: string
    Authors: string
    LCCall : LCCN option
    Subjects: string list
}
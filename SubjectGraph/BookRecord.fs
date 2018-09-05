module BookRecord

open CallNumber

type BookRecord = {
    Title: string
    Authors: string
    LCCallNum : LCCN option
    LCLetters: string option
    Subjects: string list
}

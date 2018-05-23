module BookRecord

open CallNumber

type BookRecord = {
    Title: string
    Authors: string
    LCCall : LCCN option
    Subjects: string list
}

/// Parse a Project Gutenberg RDF. TODO: move into a script. 
let gutenRDFtoBook rdfRecord = ""
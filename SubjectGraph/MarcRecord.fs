/// This will be a datatype that holds an entire marc record.
namespace Library

type MarcType = 
    | Bibliographic
    | Classification

type MarcRecord = {
    recordType : MarcType;
}

module MarcRecord = 
    // I may be losing efficiency compared to a one-pass tag-checker.
    // Maybe this should return a seq.
    let fromXML = []
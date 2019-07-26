/// Code for updating book information from online catalog APIs.
module CatalogQuery

open BookTypes
open CallNumber
open MarcXml // possibly only for output.
open Mods

let worldCatBaseURL = "http://www.worldcat.org/webservices/catalog"
let harvardBaseURL = @"https://api.lib.harvard.edu/v2/items"

let queryBookHarvard (br : BookRecord) = [] 

let compareRecords (br1 : BookRecord) (br2 : BookRecord) = 
    0.0
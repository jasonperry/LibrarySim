module SparqlQuery

open System.Web
open System.IO      (* StreamReader *)
open System.Net     (* for HTTP requests *)
open System.Xml.Linq

// TODO: read this from an .ini file THAT ISN'T SOURCE CONTROLLED! 
let endpoint = "http://35.202.98.137:3030/locsh"

// Used by all the functions that construct queries.
let queryPrefix = "PREFIX madsrdf: <http://www.loc.gov/mads/rdf/v1#>\n"
                  + "PREFIX lcsh: <http://id.loc.gov/authorities/subjects/>\n"
                  + "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> \n"


/// Type to store result of a SPARQL query as a list of (variable, value) maps.
/// Should work for any SELECT query.
type SparqlResult = {
    vars: string list;
    results: Map<string, string> list; 
} with 
    static member FromXml xmlstr = 
        let xname = XName.Get
        let srname name = XName.Get ("{http://www.w3.org/2005/sparql-results#}" + name)
        let doc = XElement.Parse xmlstr
        let varlist = (doc.Element (srname "head")).Elements (srname "variable")
                      |> List.ofSeq
        let resultlist = (doc.Element (srname "results")).Elements(srname "result")
                         |> List.ofSeq
        { 
            (* need to throw errors here? Should just return null or empty. *)
            vars = List.map 
                    (fun (elt: XElement) -> (elt.Attribute (xname "name")).Value) 
                    varlist
            (* Create a map for all the bindings in each result. *)
            results = List.map (fun (result: XElement) -> 
                                Seq.map (fun (binding: XElement) -> 
                                             ((binding.Attribute (xname "name")).Value,
                                              binding.Value)) 
                                        (result.Elements (srname "binding"))
                                |> Map.ofSeq )
                               resultlist 
        }

/// The boilerplate for sending a query. TODO: handle connection errors
/// System.Net.WebException
let sparqlQuery (queryString : string) = 
    let url = endpoint + "?query=" + HttpUtility.UrlEncode(queryString)
    let req = HttpWebRequest.Create(url) :?> HttpWebRequest
    req.Method <- "GET" (* Probably the default. *)
    req.ContentType <- "application/x-www-form-urlencoded"
    let resp = req.GetResponse() :?> HttpWebResponse
    let stream = resp.GetResponseStream ()
    let reader = new StreamReader(stream) 
    SparqlResult.FromXml (reader.ReadToEnd())
    (* parse to list of results, each with dict of variables with list of bindings?*)

// Beginning of a logging framework. 
// A singleton module is acceptable for a global object?
module Logger 

type LogLevel = DEBUG | INFO | WARNING | ERROR | FATAL
let mutable private _level = DEBUG
let mutable private _outfile = System.Console.Out
let setLevel level = _level <- level
let setOutfile f = _outfile <- f
// TODO: let these work nicely like printfs, so I can actually use them.
let Debug fmt = 
    Printf.ksprintf
        (fun s -> if _level <= DEBUG then fprintfn _outfile "[DEBUG] %s" s)
        fmt

let Info fmt = 
    Printf.ksprintf
        (fun s -> if _level <= INFO then fprintfn _outfile "[INFO] %s" s)
        fmt

let Warning fmt = 
    Printf.ksprintf
        (fun s -> if _level <= WARNING then fprintfn _outfile "[WARNING] %s" s)
        fmt

let Error fmt = 
    Printf.ksprintf
        (fun s -> if _level <= ERROR then fprintfn _outfile "[ERROR] %s" s)
        fmt

let Fatal fmt = 
    Printf.ksprintf
        (fun s -> if _level <= FATAL then fprintfn _outfile "[FATAL] %s" s)
        fmt
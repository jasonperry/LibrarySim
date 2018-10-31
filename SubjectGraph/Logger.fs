// Beginning of a logging framework. 
// A singleton module is acceptable for a global object?
module Logger 

type LogLevel = DEBUG | INFO | WARNING | ALERT | ERROR
let mutable private _level = INFO
let mutable private _outfile = System.Console.Out
let setLevel level = _level <- level
let setOutfile f = _outfile <- f
let Debug s = 
    if _level <= DEBUG then
        fprintfn _outfile "[DEBUG] %s" s
let Info s = 
    if _level <= INFO then
        fprintfn _outfile "[INFO] %s" s
let Warning s = 
    if _level <= WARNING then
        fprintfn _outfile "[WARNING] %s" s
let Alert s = 
    if _level <= ALERT then
        fprintfn _outfile "[ALERT] %s" s
let Error s = 
    if _level <= ERROR then
        fprintfn _outfile "[ERROR] %s" s
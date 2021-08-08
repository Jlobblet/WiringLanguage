module WiringLangugage.Parsers.Import

open System.IO
open FParsec
open FParsec.Pipes

[<Struct>]
type Import =
    { Filepath: string }
    static member DefaultParser: Parser<_, unit> =
        %%spaces
        -- "#import"
        -- spaces
        -- +.regexL "<.+>" "Import filepath"
        -- spaces
        -- ';'
        -|> fun fp -> { Filepath = Path.GetFullPath (fp.[1..^1]) }

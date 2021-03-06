module WiringLanguage.Parsers.Identifier

open FParsec

[<Struct>]
type Identifier =
    | Identifier of string
    static member DefaultParser: Parser<_, unit> =
        regexL @"[_a-zA-Z][_a-zA-Z0-9%]*" "Identifier" |>> Identifier
    member this.Value =
        match this with Identifier i -> i

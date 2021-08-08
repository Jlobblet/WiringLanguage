module WiringLangugage.Identifier

open FParsec

[<Struct>]
type Identifier =
    | Identifier of string
    static member DefaultParser: Parser<_, unit> =
        regex @"[_a-zA-Z][_a-zA-Z0-9]*" |>> Identifier

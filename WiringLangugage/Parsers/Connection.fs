module WiringLangugage.Connection

open FParsec
open FParsec.Pipes
open WiringLangugage.Identifier

[<Struct>]
type ConnectionPin =
    { Name: Identifier
      Pin: Identifier }
    static member DefaultParser: Parser<_, unit> =
        %% +.p<Identifier>
        -- spaces
        -- '.'
        -- spaces
        -- +.p<Identifier>
        -|> fun n p -> { Name = n; Pin = p }

[<Struct>]
type Connection =
    { Source: ConnectionPin
      Target: ConnectionPin }
    static member DefaultParser: Parser<_, unit> =
        %%spaces
        -- +.p<ConnectionPin>
        -- spaces
        -- "->"
        -- spaces
        -- +.p<ConnectionPin>
        -- spaces
        -- ';'
        -|> fun s t -> { Source = s; Target = t }

module WiringLangugage.Parsers.Connection

open FParsec
open FParsec.Pipes
open WiringLangugage.Parsers.Identifier

[<StructuredFormatDisplay("{StructuredFormatDisplay}")>]
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

    override this.ToString() =
        $"%s{this.Name.Value}.%s{this.Pin.Value}"

    member this.StructuredFormatDisplay = this.ToString()

[<StructuredFormatDisplay("{StructuredFormatDisplay}")>]
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
        <?> "connection declaration"

    override this.ToString() =
        $"Connection %A{this.Source} -> %A{this.Target};"

    member this.StructuredFormatDisplay = this.ToString()

module WiringLanguage.Parsers.Connection

open FParsec
open FParsec.Pipes
open WiringLanguage.Parsers.Identifier

[<Struct; NoComparison>]
type ConnectionDirection =
    | Forwards
    | Backwards

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
        -- +. [stringReturn "->" Forwards; stringReturn "<-" Backwards]
        -- spaces
        -- +.p<ConnectionPin>
        -- spaces
        -- ';'
        -|> (fun left direction right ->
            match direction with
            | Forwards -> { Source = left; Target = right }
            | Backwards -> { Source = right; Target = left })
        <?> "connection declaration"

    override this.ToString() =
        $"Connection %A{this.Source} -> %A{this.Target};"

    member this.StructuredFormatDisplay = this.ToString()

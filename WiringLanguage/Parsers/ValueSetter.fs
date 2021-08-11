module WiringLanguage.Parsers.ValueSetter

open FParsec
open FParsec.Pipes
open WiringLanguage.Parsers.Identifier
open WiringLanguage.Parsers.Strings

[<StructuredFormatDisplay("{StructuredFormatDisplay}")>]
[<Struct>]
type ValueSetter =
    { Name: Identifier
      ValueName: Identifier
      Value: string }
    static member DefaultParser: Parser<_, unit> =
        let comma () = %% "," -- spaces -|> ()

        %% spaces
        -- +. p<Identifier>
        -- spaces
        -- '.'
        -- spaces
        -- +. p<Identifier>
        -- spaces
        -- '='
        -- spaces
        -- +. EscapedStringSemicolon()
        -|> fun name valueName value ->
                { Name = name
                  ValueName = valueName
                  Value = value }
        <?> "value setter"

    override this.ToString() =
        $"%s{this.Name.Value}.%s{this.ValueName.Value} = %s{this.Value}"

    member this.StructuredFormatDisplay = this.ToString()


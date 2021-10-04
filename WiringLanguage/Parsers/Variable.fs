module WiringLanguage.Parsers.Variable

open FParsec
open FParsec.Pipes
open WiringLanguage.Parsers.Identifier
open WiringLanguage.Parsers.VariableType

[<StructuredFormatDisplay("{StructuredFormatDisplay}")>]
[<Struct>]
type Variable =
    { ComponentIdentifier: Identifier
      VariableType: VariableType
      Name: Identifier }
    static member DefaultParser: Parser<_, unit> =
        let comma () = %% "," -- spaces -|> ()

        %%spaces
        -- +.p<VariableType>
        -- spaces
        -- +.p<Identifier>
        -- spaces1
        -- +.(qty.[1..] / comma () * p<Identifier>)
        -- ';'
        -|> fun variableType identifier names ->
                names
                |> Array.ofSeq
                |> Array.map
                    (fun n ->
                        { ComponentIdentifier = identifier
                          VariableType = variableType
                          Name = n })
        <?> "variable declaration"

    override this.ToString() =
        $"%A{this.ComponentIdentifier} %A{this.Name}"

    member this.StructuredFormatDisplay = this.ToString()

module WiringLangugage.Parsers.Variable

open FParsec
open FParsec.Pipes
open WiringLangugage.Parsers.Identifier

[<StructuredFormatDisplay("{StructuredFormatDisplay}")>]
[<Struct>]
type Variable =
    { ComponentIdentifier: Identifier
      Name: Identifier }
    static member DefaultParser: Parser<_, unit> =
        let comma () = %% "," -- spaces -|> ()

        %% +.p<Identifier>
        -- spaces1
        -- +.(qty.[1..] / comma () * p<Identifier>)
        -- ';'
        -|> fun ``type`` names ->
                names
                |> Array.ofSeq
                |> Array.map
                    (fun n ->
                        { ComponentIdentifier = ``type``
                          Name = n })
        <?> "variable declaration"
    override this.ToString() = $"%A{this.ComponentIdentifier} %A{this.Name}"
    member this.StructuredFormatDisplay = this.ToString()

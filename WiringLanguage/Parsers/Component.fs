module WiringLanguage.Parsers.Component

open FParsec
open FParsec.Pipes
open WiringLanguage.Parsers.Identifier

[<StructuredFormatDisplay("{StructuredFormatDisplay}")>]
[<Struct>]
type ComponentField =
    | Input of input: Identifier
    | Output of output: Identifier
    | Value of value: Identifier
    static member DefaultParser: Parser<_, unit> =
        %%spaces
        -- +. [stringReturn "input" Input; stringReturn "output" Output; stringReturn "value" Value]
        -- spaces1
        -- +. p<Identifier>
        -- spaces
        -- ';'
        -|> id
    override this.ToString() =
        match this with
        | Input i -> $"input %s{i.Value};"
        | Output o -> $"output %s{o.Value};"
        | Value v -> $"value %s{v.Value}"

[<StructuredFormatDisplay("{StructuredFormatDisplay}")>]
type Component =
    { Name: Identifier
      Identifier: Identifier
      Inputs: Set<Identifier>
      Outputs: Set<Identifier>
      Values: Set<Identifier> }
    static member DefaultParser: Parser<_, unit> =
        %% spaces
        -- "component"
        -- spaces1
        -- +. p<Identifier>
        -- spaces
        -- ':'
        -- spaces
        -- +. p<Identifier>
        -- spaces
        -- '{'
        -- +.((attempt %p<ComponentField>) * qty.[0..])
        -- spaces
        -- '}'
        -|> fun name identifier fields ->
                let fields = Array.ofSeq fields
                let inputs =
                    fields
                    |> Array.choose
                        (function
                        | Input i -> Some i
                        | _ -> None)
                    |> Set.ofArray

                let outputs =
                    fields
                    |> Array.choose
                        (function
                        | Output o -> Some o
                        | _ -> None)
                    |> Set.ofArray
                    
                let values =
                    fields
                    |> Array.choose
                        (function
                        | Value v -> Some v
                        | _ -> None)
                    |> Set.ofArray

                { Name = name
                  Identifier = identifier
                  Inputs = inputs
                  Outputs = outputs
                  Values = values }
        <?> "component definition"
    override this.ToString() =
        seq {
            yield $"component %A{this.Name} : %A{this.Identifier} {{"
            let makeField name = Seq.map (fun i -> $"    %A{i}")
            yield! this.Inputs |> makeField "input"
            yield! this.Outputs |> makeField "output"
            yield! this.Values |> makeField "value"
            yield "}"
        } |> String.concat "\n"
    member this.StructuredFormatDisplay = this.ToString()

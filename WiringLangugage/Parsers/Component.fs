module WiringLangugage.Component

open WiringLangugage.Identifier
open FParsec
open FParsec.Pipes

[<Struct>]
type ComponentField =
    | Input of input: Identifier
    | Output of output: Identifier
    | Value of value: Identifier
    static member DefaultParser: Parser<_, unit> =
        %%spaces
        ?- +. [stringReturn "input" Input; stringReturn "output" Output]
        -- spaces1
        -- +. p<Identifier>
        -- spaces
        -? ';'
        -|> id

type Component =
    { Name: Identifier
      Inputs: Set<Identifier>
      Outputs: Set<Identifier>
      Values: Set<Identifier> }
    static member DefaultParser: Parser<_, unit> =
        %% spaces
        -- "component"
        -- spaces1
        -- +. %p<Identifier>
        -- spaces
        -- '{'
        -- +.(%p<ComponentField> * qty.[0..])
        -- spaces
        -- '}'
        -|> fun name fields ->
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
                  Inputs = inputs
                  Outputs = outputs
                  Values = values }

module WiringLanguage.Instance

open FSharpPlus
open WiringLanguage.Utils
open WiringLanguage.Parsers.Component
open WiringLanguage.Parsers.Identifier
open WiringLanguage.Parsers.VariableType

[<StructuredFormatDisplay("{StructuredFormatDisplay}")>]
type Instance =
    { Component: Component
      ComponentType: ComponentType
      Inputs: Set<Identifier>
      Outputs: Set<Identifier>
      Values: Map<Identifier, string Option> }
    member this.StructuredFormatDisplay =
        $"""Instance
{{ Component = %s{this.Component.Identifier.Value}
   ComponentType = %A{this.ComponentType}
   Inputs = %A{this.Inputs}
   Outputs = %A{this.Outputs}
   Values = %A{this.Values
               |> Seq.choose
                   (fun kvp ->
                       kvp.Value
                       |> map (fun v -> $"%s{kvp.Key.Value} = {v}"))
               |> List.ofSeq} }}"""

[<RequireQualifiedAccess>]
module Instance =
    let create (``component``: Component) =
        let initialiseMap v = map (fun i -> i, v) >> Map.ofSeq

        { Component = ``component``
          ComponentType = IntermediateComponent
          Inputs = ``component``.Inputs
          Outputs = ``component``.Outputs
          Values = ``component``.Values |> initialiseMap None }

    let setValue name value (instance: Instance) =
        { instance with
              Values = Map.add name value instance.Values }

    let trySetValue name value instance =
        monad.plus {
            let! newValues =
                Map.tryUpdate name value instance.Values
                |> Result.ofOption $"Could not find value %A{name} in instance %A{instance}"

            { instance with Values = newValues }
        }

    let setComponentType componentType instance =
        { instance with
              ComponentType = componentType }

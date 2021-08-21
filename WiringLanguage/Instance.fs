module WiringLanguage.Instance

open FSharpPlus
open WiringLanguage.Utils
open WiringLanguage.Parsers.Component
open WiringLanguage.Parsers.Identifier

[<StructuredFormatDisplay("{StructuredFormatDisplay}")>]
type Instance =
    { Component: Component
      Inputs: Set<Identifier>
      Outputs: Set<Identifier>
//      Inputs: Map<Identifier, Set<ConnectionPin>>
//      Outputs: Map<Identifier, Set<ConnectionPin>>
      Values: Map<Identifier, string Option> }
    member this.StructuredFormatDisplay =
        $"""Instance
{{ Component = %s{this.Component.Identifier.Value}
   Inputs = %A{this.Inputs}
   Outputs = %A{this.Outputs}
   Values = %A{this.Values
               |> Seq.choose
                   (fun kvp ->
                       kvp.Value
                       |> Option.map (fun v -> $"%s{kvp.Key.Value} = {v}"))
               |> List.ofSeq} }}"""

[<RequireQualifiedAccess>]
module Instance =
    let create (``component``: Component) =
        let initialiseMap v = Seq.map (fun i -> i, v) >> Map.ofSeq

        { Component = ``component``
          Inputs = ``component``.Inputs
          Outputs = ``component``.Outputs
          Values = ``component``.Values |> initialiseMap None }

//    let addInput name connectionPin instance =
//        let newConnections =
//            Set.add connectionPin instance.Inputs.[name]
//
//        { instance with
//              Inputs = Map.add name newConnections instance.Inputs }
//
//    let tryAddInput name connectionPin instance =
//        monad.plus {
//            let inputs = instance.Inputs
//
//            let! connections =
//                Map.tryFind name inputs
//                |> Result.ofOption $"Could not find input pin %A{name} in instance %A{instance}"
//
//            let newConnections = Set.add connectionPin connections
//
//            { instance with
//                  Inputs = Map.add name newConnections inputs }
//        }
//
//    let addOutput name connectionPin instance =
//        let newConnections =
//            Set.add connectionPin instance.Outputs.[name]
//
//        { instance with
//              Outputs = Map.add name newConnections instance.Outputs }
//
//    let tryAddOutput name connectionPin instance =
//        monad.plus {
//            let outputs = instance.Outputs
//
//            let! connections =
//                Map.tryFind name outputs
//                |> Result.ofOption $"Could not find output pin %A{name} in instance %A{instance}"
//
//            let newConnections = Set.add connectionPin connections
//
//            { instance with
//                  Outputs = Map.add name newConnections outputs }
//        }

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

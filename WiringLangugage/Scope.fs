module WiringLangugage.Scope

open FSharpPlus
open FSharpPlus.Lens
open WiringLangugage.Component
open WiringLangugage.Connection
open WiringLangugage.Identifier
open WiringLangugage.Utils
open WiringLangugage.Variable

type Instance =
    { Component: Component
      Inputs: Map<Identifier, Set<ConnectionPin>>
      Outputs: Map<Identifier, Set<ConnectionPin>>
      Values: Map<Identifier, string> }

[<RequireQualifiedAccess>]
module Instance =
    let create (``component``: Component) =
        let initialiseMap v = Seq.map (fun i -> i, v) >> Map.ofSeq

        { Component = ``component``
          Inputs = ``component``.Inputs |> initialiseMap Set.empty
          Outputs = ``component``.Outputs |> initialiseMap Set.empty
          Values = ``component``.Values |> initialiseMap "" }

    let addInput name connectionPin instance =
        let newConnections =
            Set.add connectionPin instance.Inputs.[name]

        { instance with
              Inputs = Map.add name newConnections instance.Inputs }

    let tryAddInput name connectionPin instance =
        monad.plus {
            let inputs = instance.Inputs

            let! connections =
                Map.tryFind name inputs
                |> Result.ofOption $"Could not find input pin %A{name} in instance %A{instance}"

            let newConnections = Set.add connectionPin connections

            { instance with
                  Inputs = Map.add name newConnections inputs }
        }

    let addOutput name connectionPin instance =
        let newConnections =
            Set.add connectionPin instance.Outputs.[name]

        { instance with
              Outputs = Map.add name newConnections instance.Outputs }

    let tryAddOutput name connectionPin instance =
        monad.plus {
            let outputs = instance.Outputs

            let! connections =
                Map.tryFind name outputs
                |> Result.ofOption $"Could not find output pin %A{name} in instance %A{instance}"

            let newConnections = Set.add connectionPin connections

            { instance with
                  Outputs = Map.add name newConnections outputs }
        }

    let setValue name value (instance: Instance) =
        { instance with
              Values = Map.add name value instance.Values }

    let trySetValues name value instance =
        monad.plus {
            let! newValues =
                Map.tryUpdate name value instance.Values
                |> Result.ofOption $"Could not find value $A{name} in instance $A{instance}"

            { instance with Values = newValues }
        }
//    let inline _inputs func instance =
//        func instance.Inputs <&> fun x -> { instance with Inputs = x }
//
//    let inline _input func name instance =
//        func (instance ^. _inputs).[name] <&> fun x -> { instance with Inputs = Map.add name x (instance ^. _inputs) }
//
//    let inline addInput name connection = over _input (Set.add connection)
//
//    let inline _outputs func instance =
//        func instance.Outputs <&> fun x -> { instance with Outputs = x }
//
//    let inline _values func instance =
//        func instance.Values <&> fun x -> { instance with Values = x }
//
//    let setValue identifier value = over _values (Map.add identifier value)

type Scope =
    { Components: Map<Identifier, Component>
      Instances: Map<Identifier, Instance> }

[<RequireQualifiedAccess>]
module Scope =
    let empty =
        { Components = Map.empty
          Instances = Map.empty }

    let tryFindInstance name scope =
        Map.tryFind name scope.Instances
        |> Result.ofOption $"Could not find instance %A{name} in scope"

    let addComponent (comp: Component) scope =
        { scope with
              Components = Map.add comp.Name comp scope.Components }

    let tryCreateInstance variable scope =
        Map.tryFind variable.ComponentIdentifier scope.Components
        |> Result.ofOption $"Could not find component %A{variable.ComponentIdentifier} in scope"
        |> Result.map
            (fun comp ->
                { scope with
                      Instances = Map.add variable.Name (Instance.create comp) scope.Instances })

    let tryCreateInstances variables scope =
        let folder acc var =
            acc |> Result.bind (tryCreateInstance var)

        Array.fold folder (Result.Ok scope) variables

    let tryAddConnection connection scope =
        monad.plus {
            let! source = tryFindInstance connection.Source.Name scope
            let! newSource = Instance.tryAddOutput connection.Source.Pin connection.Target source

            let! target = tryFindInstance connection.Target.Name scope
            let! newTarget = Instance.tryAddInput connection.Target.Pin connection.Source target

            // The keys are guaranteed to exist from tryFindInstance, so this won't add new keys to the maps
            let newInstances =
                scope
                    .Instances
                    .Add(connection.Source.Name, newSource)
                    .Add(connection.Target.Name, newTarget)

            { scope with Instances = newInstances }
        }

//    let inline _components func scope =
//        func scope.Components <&> fun x -> { scope with Components = x }
//
//    let addComponent comp = over _components (Set.add comp)
//
//    let inline _instances func scope =
//        func scope.Instances <&> fun x -> { scope with Instances = x }
//
//    let addInstance name instance = over _instances (Map.add name instance)
//
//    let inline _instance func name scope =
//        func (scope ^. _instances).[name] <&> fun x -> { scope with Instances = Map.add name x (scope ^. _instances) }
//
//    let inline _instanceInputs func = _instance << Instance._inputs <| func
//
//    let inline _instanceOutputs func = _instance << Instance._outputs <| func
//
//    let inline _instanceValues func = _instance << Instance._values <| func

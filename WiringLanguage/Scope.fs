module WiringLanguage.Scope

open FSharpPlus
open WiringLanguage.Parsers.ValueSetter
open WiringLanguage.Utils
open WiringLanguage.Parsers.Component
open WiringLanguage.Parsers.Connection
open WiringLanguage.Parsers.Identifier
open WiringLanguage.Parsers.Variable

[<StructuredFormatDisplay("{StructuredFormatDisplay}")>]
type Instance =
    { Component: Component
      Inputs: Map<Identifier, Set<ConnectionPin>>
      Outputs: Map<Identifier, Set<ConnectionPin>>
      Values: Map<Identifier, string Option> }
    member this.StructuredFormatDisplay =
        $"""Instance
{{ Component = %s{this.Component.Identifier.Value}
   Inputs = %A{this.Inputs
               |> Map.filter (fun _ -> not << Set.isEmpty)
               |> Seq.map (fun kvp -> $"%s{kvp.Key.Value} <- %A{kvp.Value}")
               |> List.ofSeq}
   Outputs = %A{this.Outputs
                |> Map.filter (fun _ -> not << Set.isEmpty)
                |> Seq.map (fun kvp -> $"%s{kvp.Key.Value} -> %A{kvp.Value}")
                |> List.ofSeq}
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
          Inputs = ``component``.Inputs |> initialiseMap Set.empty
          Outputs = ``component``.Outputs |> initialiseMap Set.empty
          Values = ``component``.Values |> initialiseMap None }

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

    let trySetValue name value instance =
        monad.plus {
            let! newValues =
                Map.tryUpdate name value instance.Values
                |> Result.ofOption $"Could not find value %A{name} in instance %A{instance}"

            { instance with Values = newValues }
        }

[<StructuredFormatDisplay("{StructuredFormatDisplay}")>]
type Scope =
    { Components: Map<Identifier, Component>
      Instances: Map<Identifier, Instance>
      AnonymousInstances: Instance list }
    member this.StructuredFormatDisplay =
        $"""Scope
{{ Components = %A{this.Components
                   |> Map.keys
                   |> Seq.map (fun i -> i.Value)
                   |> List.ofSeq}
   Instances = %A{this.Instances}
   AnonymousInstances = %A{this.AnonymousInstances} }}"""

[<RequireQualifiedAccess>]
module Scope =
    let empty =
        { Components = Map.empty
          Instances = Map.empty
          AnonymousInstances = List.empty }

    let union this other =
        { Components = Map.union other.Components this.Components
          Instances = Map.union other.Instances this.Instances
          AnonymousInstances = List.append other.AnonymousInstances this.AnonymousInstances }

    let tryFindInstance name scope =
        Map.tryFind name scope.Instances
        |> Result.ofOption $"Could not find instance %A{name} in scope"

    let tryUpdateInstance name newInstance scope =
        monad.plus {
            let! newInstances =
                Map.tryUpdate name newInstance scope.Instances
                |> Result.ofOption $"Could not find instance %A{name} in scope"

            return { scope with Instances = newInstances }
        }

    let addComponent (comp: Component) scope =
        { scope with
              Components = Map.add comp.Name comp scope.Components }

    let addAnonymousInstance instance scope =
        { scope with
              AnonymousInstances = instance :: scope.AnonymousInstances }
        
    let anonymiseInstance identifier scope =
        match Map.tryPop identifier scope.Instances with
        | None -> scope
        | Some(newInstances, instance) -> { scope with Instances = newInstances; AnonymousInstances = instance :: scope.AnonymousInstances }

    let tryCreateInstance variable scope =
        Map.tryFind variable.ComponentIdentifier scope.Components
        |> Result.ofOption $"Could not find component %A{variable.ComponentIdentifier} in scope"
        |> Result.map
            (fun comp ->
                match variable.Name with
                | Identifier "_" -> addAnonymousInstance (Instance.create comp) scope
                | name ->
                    { scope with
                          Instances = Map.add name (Instance.create comp) scope.Instances })

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
        |> Result.mapError (fun e -> $"Error in %A{connection}: %s{e}")

    let trySetValue (value: ValueSetter) scope =
        monad.plus {
            let! instance = tryFindInstance value.Name scope
            let! newInstance = Instance.trySetValue value.ValueName (Some value.Value) instance
            return! tryUpdateInstance value.Name newInstance scope
        }

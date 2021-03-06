// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.IO
open System.Reflection
open System.Xml.Linq
open FSharp.XExtensions
open FSharpPlus
open FSharpPlus.Data
open WiringLanguage.StandardLibrary.Settings

type Component =
    { Name: string
      Identifier: string
      Inputs: string []
      Outputs: string []
      Values: string [] }

let tryGetValue (element: XElement) =
    match String.IsNullOrWhiteSpace element.Value with
    | true -> None
    | false -> Some element.Value

let tryGetAttributeValue (name: string) (element: XElement) =
    match element.Attribute name with
    | null -> None
    | a -> Some a.Value

let tryGetName (language: XDocument) (xe: XElement) =
    tryGetAttributeValue "nameidentifier" xe
    |> Option.orElseWith (fun () -> tryGetAttributeValue "identifier" xe)
    |> bind
        (fun i ->
            language.Root.Element $"entityname.%s{i.ToLowerInvariant()}"
            |> tryGetValue)

let tryGetItems barotraumaLocation (filepath: string) =
    let full =
        Path.Combine(barotraumaLocation, filepath)

    match File.Exists full with
    | false -> None
    | true -> XDocument.Load(full).Root.Elements("Item") |> Some

let tryGetValues (assembly: Assembly) (xElement: XElement) =
    monad.plus {
        let! typeName =
            xElement.Elements()
            |> Seq.tryPick
                (fun xe ->
                    let name = xe.Name.LocalName

                    match name.Contains("Component", StringComparison.OrdinalIgnoreCase) with
                    | true -> Some name
                    | false -> None)

        let! serializeAttribute =
            assembly.GetType("Barotrauma.Serialize")
            |> Option.ofObj


        let! componentType =
            assembly.GetType($"Barotrauma.Items.Components.%s{typeName}", false, true)
            |> Option.ofObj

        let values =
            componentType.GetProperties(
                BindingFlags.Instance
                ||| BindingFlags.Public
                ||| BindingFlags.NonPublic
                ||| BindingFlags.FlattenHierarchy
            )
            |> choose (
                Option.protect
                    (fun propertyInfo ->
                        let hasSerialize =
                            propertyInfo.IsDefined serializeAttribute

                        match hasSerialize with
                        | false -> None
                        | true -> Some propertyInfo.Name)
                >> Option.flatten
            )

        return values
    }

let extractComponents assembly english (xElement: XElement) =
    match xElement.Element "ConnectionPanel" with
    | null -> None
    | connectionPanel ->
        monad.plus {
            // Get the identifier of the item
            let! identifier = tryGetAttributeValue "identifier" xElement

            let name =
                tryGetName english xElement
                |> Option.defaultValue identifier
                |> String.replace " " ""

            // Get inputs from the connection panel
            let inputs =
                connectionPanel.Elements "input"
                |> choose (tryGetAttributeValue "name")
                |> Array.ofSeq

            // Get outputs from the connection panel
            let outputs =
                connectionPanel.Elements "output"
                |> choose (tryGetAttributeValue "name")
                |> Array.ofSeq

            // Reflection to find serializable things
            let values =
                tryGetValues assembly xElement
                |> Option.defaultValue [||]

            { Name = name
              Identifier = identifier
              Inputs = inputs
              Outputs = outputs
              Values = values }
        }

let generateComponentDefinition ``component`` =
    seq {
        let identifier = ``component``.Identifier

        let name =
            ``component``
                .Name
                .Replace("component", "", StringComparison.OrdinalIgnoreCase)
                .Replace("detector", "Detector", StringComparison.OrdinalIgnoreCase)
            |> mapi
                (function
                | 0 -> Char.ToUpper
                | _ -> id)
            |> Array.ofSeq
            |> String

        yield $"component %s{name} : %s{identifier} {{"

        let makeField name ns =
            map (fun n -> $"    %s{name} %s{n};") ns

        yield! makeField "input" ``component``.Inputs
        yield! makeField "output" ``component``.Outputs
        yield! makeField "value" ``component``.Values
        yield "}"
    }
    |> String.concat "\n"

[<EntryPoint>]
let main argv =
    let settings = Settings.FromArgv argv

    let assembly =
        Path.Combine(settings.BarotraumaLocation, "DedicatedServer.dll")
        |> Assembly.LoadFile

    let english =
        Path.Combine(settings.BarotraumaLocation, "Content", "Texts", "English", "EnglishVanilla.xml")
        |> XDocument.Load

    let components =
        Path.Combine(settings.BarotraumaLocation, "Data", "ContentPackages", "Vanilla 0.9.xml")
        |> XDocument.Load
        |> fun d -> d.Root.Elements "Item"
        |> choose (tryGetAttributeValue "file")
        |> choose (tryGetItems settings.BarotraumaLocation)
        |> Seq.collect id
        |> choose (extractComponents assembly english)
        |> groupBy (fun c -> c.Name)
        |> Seq.collect
            (fun (_, cs) ->
                match Seq.length cs with
                | 1 -> cs
                | _ ->
                    cs
                    |> map
                        (fun c ->
                            { c with
                                  Name =
                                      match c.Name.Equals(c.Identifier, StringComparison.OrdinalIgnoreCase) with
                                      | true -> c.Name
                                      | false -> $"%s{c.Name}_%s{c.Identifier}" }))
        |> map generateComponentDefinition
        |> String.concat "\n\n"

    File.WriteAllText("standard library.wl", components + "\n")
    0 // return an integer exit code

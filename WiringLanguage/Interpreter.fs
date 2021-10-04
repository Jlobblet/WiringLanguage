module WiringLanguage.Interpreter

open System.IO
open FSharpPlus
open FParsec
open FParsec.Pipes
open WiringLanguage.Instruction
open WiringLanguage.Scope
open WiringLanguage.Utils

let rec InterpretString (string: string) =
    let folder scope instruction =
        bind
            (fun s ->
                match instruction with
                | Import i ->
                    InterpretFile i.Filepath
                    |> map (Scope.union s)
                | Variables vs -> Scope.tryCreateInstances vs s
                | ComponentDefinition comp -> Scope.addComponent comp s |> Result.Ok
                | ConnectionDefinition conn -> Scope.tryAddWire conn s
                | ValueSetter value -> Scope.trySetValue value s)
            scope

    string.Trim()
    |> run (%p<Instruction> * qty.[1..])
    |> Result.ofParseResult
    |> bind (fold folder (Result.Ok Scope.empty))

and InterpretFile = File.ReadAllText >> InterpretString

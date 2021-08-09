module WiringLangugage.Interpreter

open System.IO
open FParsec
open FParsec.Pipes
open WiringLangugage.Instruction
open WiringLangugage.Scope
open WiringLangugage.Utils

let rec InterpretString (string: string) =
    let folder scope instruction =
        Result.bind
            (fun s ->
                match instruction with
                | Import i ->
                    InterpretFile i.Filepath
                    |> Result.map (Scope.union s)
                | Variables vs -> Scope.tryCreateInstances vs s
                | ComponentDefinition comp -> Scope.addComponent comp s |> Result.Ok
                | ConnectionDefinition conn -> Scope.tryAddConnection conn s)
            scope

    string.Trim()
    |> run (%p<Instruction> * qty.[1..])
    |> Result.ofParseResult
    |> Result.bind (Seq.fold folder (Result.Ok Scope.empty))

and InterpretFile = File.ReadAllText >> InterpretString

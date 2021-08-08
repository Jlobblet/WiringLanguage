module WiringLanguage.StandardLibrary.Settings

open System
open System.IO
open Argu

type Arguments =
    | BarotraumaLocation of path: string
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | BarotraumaLocation _ -> "Path to Barotrauma's root directory"

let Parser = ArgumentParser<Arguments>()

type Settings =
    { BarotraumaLocation: string }
    static member FromArgv argv =
        let results = Parser.Parse argv

        let barotraumaLocation =
            results.GetResult <@ BarotraumaLocation @>
            |> Environment.ExpandEnvironmentVariables
            |> Path.GetFullPath

        { BarotraumaLocation = barotraumaLocation }

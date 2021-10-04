module WiringLanguage.Utils

open FSharpPlus
open FParsec

[<RequireQualifiedAccess>]
module Result =
    let ofOption err = function
            | Some v -> Result.Ok v
            | None -> Result.Error err
            
    let ofOptionWith errThunk = function
        | Some v -> Result.Ok v
        | None -> Result.Error <| errThunk()
        
    let ofParseResult = function
        | ParserResult.Success (r, u, p) -> Result.Ok r
        | ParserResult.Failure (e, u, p) -> Result.Error e

[<RequireQualifiedAccess>]
module Map =
    let tryUpdate key value map =
        match Map.containsKey key map with
        | false -> None
        | true -> Map.add key value map |> Some
        
    let tryPop key map =
        match Map.containsKey key map with
        | false -> None
        | true ->
            let value = map.[key]
            (Map.remove key map, value)|> Some

let ParserResultExpect parserResult =
    parserResult
    |> Result.ofParseResult
    |> either (fun (a, _, _) -> a) (failwithf "%A")

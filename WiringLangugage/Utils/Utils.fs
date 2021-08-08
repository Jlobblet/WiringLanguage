module WiringLangugage.Utils

open FParsec

let ParserResultExpect result =
    match result with
    | ParserResult.Success (result, state, position) -> result
    | Failure _ as f -> failwith $"%A{f}"

[<RequireQualifiedAccess>]
module Result =
    let ofOption err = function
            | Some v -> Result.Ok v
            | None -> Result.Error err
            
    let ofOptionWith errThunk = function
        | Some v -> Result.Ok v
        | None -> Result.Error <| errThunk()

[<RequireQualifiedAccess>]
module Map =
    let tryUpdate key value map =
        match Map.containsKey key map with
        | false -> None
        | true -> Map.add key value map |> Some

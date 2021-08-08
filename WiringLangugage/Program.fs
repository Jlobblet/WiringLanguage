// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open FParsec
open FParsec.Pipes
open WiringLangugage
open WiringLangugage.Component
open WiringLangugage.Identifier
open WiringLangugage.Instruction
open WiringLangugage.Scope
open WiringLangugage.Utils


[<EntryPoint>]
let main argv =
    let folder scope instruction =
        Result.bind
            (fun s ->
                match instruction with
                | Variables vs -> Scope.tryCreateInstances vs s
                | ComponentDefinition comp -> Scope.addComponent comp s |> Result.Ok
                | ConnectionDefinition conn -> Scope.tryAddConnection conn s)
            scope

    """
    component Mem {
        input signal_in;
        input lock_state;
        output signal_out;
    }
    Mem m1, m2;
    m1.signal_out -> m2.signal_in;
    """
    |> run (%p<Instruction> * qty.[0..])
    |> ParserResultExpect
    |> Seq.fold folder (Result.Ok Scope.empty)
    |> printfn "%A"

    //    "m1.signal_out -> m2.signal_in;"
//    |> run (%p<Instruction>)
//    |> printfn "%A"

    0 // return an integer exit code

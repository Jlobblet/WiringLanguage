// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open FParsec
open FParsec.Pipes
open FSharpPlus
open WiringLangugage
open WiringLangugage.Interpreter


[<EntryPoint>]
let main argv =
    """
    #import </home/jlb/Documents/Barotrauma/WiringLangugage/WiringLanguage.StandardLibrary/bin/Debug/net5.0/standard library.wl>;
    Memory m1, m2;
    m1.signal_out -> m2.signal_in;
    """
    |> InterpretString
    |> printfn "%A"

//    |> Result.get
//    |> fun s -> printfn $"%i{s.Components.Count}"
    
//    """
//component Battery : battery {
//    input power_in;
//    input set_rate;
//    output power_out;
//    output charge;
//    output charge_%;
//    output charge_rate;
//}
//"""
//    |> InterpretString
//    |> printfn "%A"

    0 // return an integer exit code

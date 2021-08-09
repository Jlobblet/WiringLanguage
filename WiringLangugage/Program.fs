// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open WiringLangugage.Interpreter


[<EntryPoint>]
let main argv =
    """
    #import </home/jlb/Documents/Barotrauma/WiringLangugage/WiringLanguage.StandardLibrary/bin/Debug/net5.0/standard library.wl>;
    Memory m1, m2, m_out;
    Adder a;
    m1.signal_out -> a.signal_in1;
    m2.signal_out -> a.signal_in2;
    a.signal_out -> m_out.signal_in;
    m1.Value = 600;
    m2.Value = 21;
    """
    |> InterpretString
    |> printfn "%A"

    0 // return an integer exit code

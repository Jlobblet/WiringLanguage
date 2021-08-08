module WiringLangugage.Variable

open WiringLangugage.Identifier
open FParsec
open FParsec.Pipes

[<Struct>]
type Variable =
    { ComponentIdentifier: Identifier
      Name: Identifier }
    static member DefaultParser: Parser<_, unit> =
        let comma () = %% "," -- spaces -|> ()

        %% +.p<Identifier>
        -- spaces1
        -- +.(qty.[1..] / comma () * p<Identifier>)
        -- ';'
        -|> fun ``type`` names ->
                names
                |> Array.ofSeq
                |> Array.map
                    (fun n ->
                        { ComponentIdentifier = ``type``
                          Name = n })

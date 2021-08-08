module WiringLangugage.Instruction

open FParsec
open FParsec.Pipes
open WiringLangugage.Variable
open WiringLangugage.Component
open WiringLangugage.Connection

type Instruction =
    | Variables of variables: Variable []
    | ComponentDefinition of ``component``: Component
    | ConnectionDefinition of connection: Connection
    static member DefaultParser: Parser<Instruction, unit> =
        %% spaces
        -- +.([ Variable.DefaultParser |>> Variables
                Component.DefaultParser |>> ComponentDefinition
                Connection.DefaultParser |>> ConnectionDefinition ]
              |> List.map attempt)
        -- spaces
        -|> id

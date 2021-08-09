module WiringLangugage.Instruction

open FParsec
open FParsec.Pipes
open WiringLangugage.Parsers.Import
open WiringLangugage.Parsers.Variable
open WiringLangugage.Parsers.Component
open WiringLangugage.Parsers.Connection

type Instruction =
    | Import of Import
    | ComponentDefinition of Component
    | Variables of Variable []
    | ConnectionDefinition of Connection
    static member DefaultParser: Parser<Instruction, unit> =
        %% spaces
        -- +.[ attempt Import.DefaultParser |>> Import
               attempt Component.DefaultParser |>> ComponentDefinition
               attempt Connection.DefaultParser |>> ConnectionDefinition
               Variable.DefaultParser |>> Variables ]
        -- spaces
        -|> id

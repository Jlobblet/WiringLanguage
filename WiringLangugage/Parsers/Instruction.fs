module WiringLangugage.Instruction

open FParsec
open FParsec.Pipes
open WiringLangugage.Parsers.Import
open WiringLangugage.Parsers.ValueSetter
open WiringLangugage.Parsers.Variable
open WiringLangugage.Parsers.Component
open WiringLangugage.Parsers.Connection

type Instruction =
    | Import of Import
    | ComponentDefinition of Component
    | Variables of Variable []
    | ConnectionDefinition of Connection
    | ValueSetter of ValueSetter
    static member DefaultParser: Parser<Instruction, unit> =
        %% spaces
        -- +.[ attempt Import.DefaultParser |>> Import
               attempt Component.DefaultParser |>> ComponentDefinition
               attempt Connection.DefaultParser |>> ConnectionDefinition
               attempt ValueSetter.DefaultParser |>> ValueSetter
               Variable.DefaultParser |>> Variables ]
        -- spaces
        -|> id

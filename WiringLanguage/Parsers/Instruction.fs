module WiringLanguage.Instruction

open FParsec
open FParsec.Pipes
open WiringLanguage.Parsers.Import
open WiringLanguage.Parsers.ValueSetter
open WiringLanguage.Parsers.Variable
open WiringLanguage.Parsers.Component
open WiringLanguage.Parsers.Connection

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
        <?> "instruction"

module WiringLanguage.Parsers.VariableType

open FParsec
open FParsec.Pipes

[<Struct>]
type VariableType =
    | InputComponent
    | IntermediateComponent
    | OutputComponent
    static member private InputOutputParser: Parser<_, unit> =
        %%spaces
        -- '('
        -- spaces
        -- +. [stringReturn "input" InputComponent; stringReturn "output" OutputComponent]
        -- spaces
        -- ')'
        -|> id
        
    static member DefaultParser: Parser<_, unit> =
        attempt VariableType.InputOutputParser <|> preturn IntermediateComponent <?> "variable or component type"
        
type ComponentType = VariableType

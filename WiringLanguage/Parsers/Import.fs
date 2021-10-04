module WiringLanguage.Parsers.Import

open System
open System.IO
open FParsec
open FParsec.Pipes

[<Struct; StructuredFormatDisplay("{StructuredFormatDisplay}")>]
type Import =
    { Filepath: string }
    static member DefaultParser: Parser<_, unit> =
        %%spaces
        -- "#import"
        -- spaces
        -- +.regexL "<.+>" "Import filepath"
        -- spaces
        -- ';'
        -|> fun fp -> { Filepath = fp.[1..^1] |> Environment.ExpandEnvironmentVariables |> Path.GetFullPath  }
        <?> "import statement"

    override this.ToString() = $"#import <%s{this.Filepath}>;"
    member this.StructuredFormatDisplay = this.ToString()

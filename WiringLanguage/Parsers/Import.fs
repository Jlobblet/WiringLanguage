module WiringLanguage.Parsers.Import

open System.IO
open FParsec
open FParsec.Pipes

[<StructuredFormatDisplay("{StructuredFormatDisplay}")>]
[<Struct>]
type Import =
    { Filepath: string }
    static member DefaultParser: Parser<_, unit> =
        %%spaces
        -- "#import"
        -- spaces
        -- +.regexL "<.+>" "Import filepath"
        -- spaces
        -- ';'
        -|> fun fp -> { Filepath = Path.GetFullPath fp.[1..^1] }
        <?> "import statement"
    override this.ToString() = $"#import <%s{this.Filepath}>;"
    member this.StructuredFormatDisplay = this.ToString()

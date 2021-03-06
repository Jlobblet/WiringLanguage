module WiringLanguage.Parsers.Strings

open FParsec

let EscapedString() = regexL @""".*?(?<!\\)(\\\\)*?""" "Escaped string" |>> (fun s -> s.[1..^1])

let EscapedStringSemicolon() = regexL @""".*?(?<!\\)(\\\\)*?"";" "Escaped string semicolon" |>> (fun s -> s.[1..^2])

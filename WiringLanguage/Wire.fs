module WiringLanguage.Wire

open WiringLanguage.Instance
open WiringLanguage.Parsers.Identifier

type Wire =
    { SourceInstance: Instance
      SourcePin: Identifier
      TargetInstance: Instance
      TargetPin: Identifier }

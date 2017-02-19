purescript-abc-parser
=====================

WORK IN PROGRESS

This is another parser for files conforming to the ABC Notation (version 2.1) for traditional music scores. It is written in PureScript, largely ported from the elm version.  The reason for the exploration of PureScript is Evan's seeming intention to [drop support for the applicative style](https://groups.google.com/forum/#!topic/elm-dev/0AHSnDdkSkQ) and the chaos that is likely to ensue for users of Bogdan Popa's [elm-combine](https://github.com/Bogdanp/elm-combine).

This parser relies heavily on a [regex combinator](https://github.com/purescript-contrib/purescript-string-parsers/issues) which is currently missing from purescript-string-parsers. A prototype it is included in this project with a view to incorporating it later on into the parser combinator library proper.

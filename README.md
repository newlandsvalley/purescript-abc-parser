purescript-abc-parser
=====================

This is another parser for input conforming to the ABC Notation (version 2.1) for traditional music scores. It is written in PureScript, largely ported from the elm version.  The reason for the exploration of PureScript is Evan's seeming intention to [drop support for the applicative style](https://groups.google.com/forum/#!topic/elm-dev/0AHSnDdkSkQ) and the chaos that is likely to ensue for users of Bogdan Popa's [elm-combine](https://github.com/Bogdanp/elm-combine).

The parser is intended to be functionally identical to the Elm one. The main differences between the combinator libraries is that elm-combine did not implement the __try__ combinator and appears to automatically backtrack when, for example, more than one option within a __choice__ start with the same lexeme.  In the purescript string parser, this situation is ambiguous and has to be resolved through use of __try__.

The parser itself is largely complete.  All the Elm tests have been transferred. There is still work to be done to complete the translation of ABC to a MIDI recording.

purescript-abc-parser
=====================

[![Latest release](http://img.shields.io/github/release/newlandsvalley/purescript-abc-parser.svg)](https://github.com/newlandsvalley/purescript-abc-parser/releases)

[![Build Status](https://github.com/newlandsvalley/purescript-abc-parser/workflows/CI/badge.svg)](https://github.com/newlandsvalley/purescript-abc-parser/actions)


This is a parser for version 2.1 of Chris Walshaw's [ABC Notation](http://abcnotation.com/) which is primarily designed as an interchange format for scores of traditional music.  Also included are functions to manipulate the parse tree in order to provide alteration of tempo, transposition, conversion to MIDI etc.

For more information, see the [guide](https://github.com/newlandsvalley/purescript-abc-parser/blob/master/docs/GUIDE.md).

Motivation
----------

The goal of this project is not to produce a general purpose parser for all forms of music in a wide variety of computational settings.  Rather, it is to provide a tool that will parse an individual traditional tune when presented to it in a browser - either from a file or from keyed input. In particular, the parser is designed to handle the majority of tunes housed in the major Western European collections - particularly [The Session](https://thesession.org/), [FolkWiki](http://www.folkwiki.se/), [Spillefolk](https://spillefolk.dk/nodesamlingen/) and [abcnotation.com](http://abcnotation.com/).

 Consequently, aspects of the spec that apply to other settings or other musical forms will be ignored or curtailed.  In addition, parts of the specification marked as _volatile_ will be treated as being non-normative and in some cases ignored. 

It is assumed that it will work in cooperation with other modules which will be responsible for such aspects as editing, displaying or playing the score. It is a particular design aim to support editor applications such that a user may, if she prefers, edit the tune body before even thinking about the headers. 

Support for ABC Version 2.2
---------------------------

As far as I can tell, ABC version 2.2 is also supported. Unfortunately, very many sections of this spec are still marked as _volatile_.

The main changes in the spec are to do with multiple voices and in particular, the manner in which clefs for a variety of (possibly transposing) instruments may be represented. This is not a problem for most traditional music collections.  In this parser, clef descriptions (and all other voice properties) are parsed, but left predominantly untyped.

Support for Polyphony
---------------------

There is a degree of support for polyphony in the ```Voice``` module.  If the ABC contains multiple ```V:``` (voice) headers, then it gives a separate ABC tune for each voice.  These can then be passed to a suitable [polyphonic player](https://github.com/newlandsvalley/purescript-school-of-music/tree/master/polyphonic-player). However, the ```Midi``` module remains monophonic.

Deviations from the spec
------------------------

  * Tunebooks.  Only one tune is allowed per file containing text entirely dedicated to that tune. Comsequently the need for ```free text``` or ```embdedded fragments``` does not arise.
  * Typeset text.  Not supported.  It is assumed that any associated score-engraving software will include its own typesetting strategy.
  * Chord Symbols.  Parsed but ignored (intentionally) in the MIDI module.  These tend to sound terrible and, in my opinion, tend to be too dictatorial.
  * Decorations are supported against bar lines, notes and chords but are not currently supported against (the start of) tuplets or rests. You may, of course, decorate any note in a tuplet.
  * Mandatory information fields (headers).  In an editor application, it is important to allow the user the option of first entering the notes and only later the information fields, whilst parsing the input after each keystroke.  For this reason, mandatory headers are not enforced - it is assumed that later software modules will enforce them in many circumstances.  In particular, the ```X:(reference-number)``` header has no usefulness in a browser setting.
  * Unicode escape sequences.  Browsers have full Unicode support and I would expect users to use fully unicode aware editors these days and so this feature is ignored.
  

Issues
------

* Slurs (represented by round brackets) are awkward. They seem to be impossible to match - for instance they can span across bars or even across separate lines of music.  I attach them directly to the notes (or note groups) that delineate the slur. However, where the slur is not directly attached to the note (e.g. when attached to a broken rhythm operator) then the parser is lenient, accepting but discarding the slur bracket. Where a note is prefaced both by grace note(s) and an opening slur then the grace note must come before the slur bracket.
* I have found no description of how a tuplet should be validated. Currently, tuplets must be completely contained within a bar and the number of items in the tuplet must agree with the its signature.  Spaces are allowed between the notes but tuplets may not be embedded, one inside the other.
* Grace notes are not supported against chords.  (I am unclear what the specification defines here with respect to grace notes and see note above.)
* Grace notes are, however, supported against notes in all other contexts and attached to them directly, although optionally mediated by a left slur bracket. 
* In translating to MIDI, only a single voice is recognized.

To Build
--------

    npm run build 

To Test
-------

    npm run test


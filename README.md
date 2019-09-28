purescript-abc-parser
=====================

[![Latest release](http://img.shields.io/github/release/newlandsvalley/purescript-abc-parser.svg)](https://github.com/newlandsvalley/purescript-abc-parser/releases)
[![Build Status](https://travis-ci.org/newlandsvalley/purescript-abc-parser.svg?branch=master)](https://travis-ci.org/newlandsvalley/purescript-abc-parser)


This is a parser for Chris Walshaw's [ABC Notation](http://abcnotation.com/) which is primarily designed as an interchange format for scores of traditional music.  Also included are functions to manipulate the parse tree in order to provide alteration of tempo, transposition, conversion to MIDI etc.

Features
--------

*  The parser is primarily aimed at web applications that deal with single voice traditional music.
*  It is biased towards editor applications in that it attempts to be as lenient as possible whilst still honouring the intentions of the ABC specification. In particular, there is no requirement for any header to be present at all - sensible defaults are used instead.  This means that an editor application can allow the user (if she prefers) to concentrate on the notes and only add the headers at a later stage.
*  It attempts to be helpful to score-engraving software.  For example, bars are first class entities in the ABC ADT; grace notes are directly attached to the notes that they 'grace'; line continuations do actually join the two lines in question.
*  It attempts to be helpful to player applications in that it provides a translation to MIDI.
*  There is limited support for polyphony.  The parser recognizes 'V' voice headers which may introduce each polyphonic voice.  The __Voices__ module allows the tune body to be partitioned into separate bodies for each voice if a score line is introduced by a Voice inline field (See section 7.3 of the ABC specification).  However, the MIDI module ignores these Voice headers and thus does not support polyphony

Issues
------

* I am unhappy about the way in which the specification defines slurs (represented by round brackets). These seem to be impossible to match - for instance they can span across bars or even across separate lines of music.  I treat them as free-floating entities.
* I am also unhappy about **order of constructs** (4.20).  A decoration in this ordering has to decorate an individual note.  But there is a clear requirement in many cases (such as codas) to decorate a note group such as a chord or tuplet.
* I have found no description of how a tuplet should be validated. Currently, tuplets must be completely contained within a bar and the number of items in the tuplet must agree with the its signature.  Spaces are allowed between the notes but tuplets may not be embedded, one inside the other.
* Grace notes and decorations are not supported against chords.  (I am unclear what the specification defines here with respect to grace notes and see note above.)
* Grace notes are, however, supported against notes in all other contexts and attached to them directly. This would otherwise mean that they cannot be separated by a slur but we accept a slur here and throw it away. We also do this for a slur before the second note in a broken-rhythm pair. 
* In translating to MIDI, only a single voice is recognized and if voltas are present only the first and second are recognized (third and fourth are ignored).




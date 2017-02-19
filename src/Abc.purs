module Abc
        ( parse
        , parseKeySignature
        ) where

import Prelude (($), (<$>), (<$), (<*>), (<*), (*>), (==), (<>), (+), (-), (/), join)
import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.List (length, singleton) as List
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.Utils (length, filter, startsWith, mapChars, includes)
import Data.String (toUpper, charAt, singleton)
import Data.Int (fromString, pow)
import Data.Foldable (foldr, foldMap)
import Data.Functor (map)
import Data.Rational (Rational, fromInt, (%))
import Data.Rational (rational) as Rational
import Data.Tuple (Tuple(..))
import Text.Parsing.StringParser (Parser, ParseError, runParser, try)
import Text.Parsing.StringParser.String (satisfy, string, char, eof)
import Text.Parsing.StringParser.Combinators (between, choice, fix, many, many1, manyTill, option, optionMaybe, (<?>))


import Debug.Trace (trace)

import ParserExtra (regex)

import Abc.ParseTree

traceParse :: forall a. String -> a -> a
traceParse s p =
  trace s (\_ -> p)

abc :: Parser AbcTune
abc =
    buildAbcTune <$> headers <*> body

body :: Parser (List BodyPart)
body =
    (:)
        <$> score
        <*> manyTill1
                -- (leftBiasedOr score tuneBodyHeader)
                (score <|> tuneBodyHeader)
                eof

score :: Parser BodyPart
score =
    Score <$> manyTill1 scoreItem eol

scoreItem :: Parser Music
scoreItem =
  choice
    [
      continuation
    , ignore
    , spacer
    , decoration
    , chordSymbol
    , annotation
    , graceNote
    , try tuplet
    , slur
    , rest
    , try brokenRhythmPair -- must place 'before' note because of potential ambiguity of AbcNote
    , note
    , barline
    , inline
    , chord
    ]
      <?> "score item"

{-
    fix
        \i ->
            -- log "score item" <$>
            (choice
                [ chord
                , inline
                , barline
                , brokenRhythmPair
                  -- must place before note because of potential ambiguity of AbcNote
                , note
                , rest
                , tuplet
                , slur
                , graceNote
                  -- we are not enforcing the ordering of grace notes, chords etc pre-note
                , annotation
                , chordSymbol
                , decoration
                , spacer
                , ignore
                , continuation
                ]
            )
                <?> "score item"
-}

chord :: Parser Music
chord =
    Chord
        <$> abcChord
        <?> "chord"

abcChord :: Parser AbcChord
abcChord =
    buildChord
        <$> (between (char '[') (char ']') (many1 abcNote))
        <*> optionMaybe noteDur
        <?> "ABC chord"


inline :: Parser Music
inline =
    Inline
        <$> between (char '[') (char ']') (tuneBodyInfo true)
        <?> "inline header"

barline :: Parser Music
barline =
  traceParse "barline" <$>
    choice
        [
          normalBarline
        , degenerateBarRepeat
        ]
{- a normal bar line (plus optional repeat iteration marker)
   see comments in 4.8 Repeat/bar symbols:
   Abc parsers should be quite liberal in recognizing bar lines. In the wild, bar lines may have
   any shape, using a sequence of | (thin bar line), [| or |] (thick bar line), and : (dots), e.g. |[| or [|:::
-}
normalBarline :: Parser Music
normalBarline =
    buildBarline
        <$> barSeparator
        <*> optionMaybe repeatSection
        <?> "barline"

{- sometimes in the wild we get a degenerate repeat marker at the start of a line of music like this:
     [1 .....
   or
     _[1 ....
   again we have to be careful about ambiguity between this and inline headers by making sure we parse '[' immediately followed by '1' etc.
-}
degenerateBarRepeat :: Parser Music
degenerateBarRepeat =
    Barline
        <$> (buildBar Thin Nothing
                <$> (Just
                        <$> (whiteSpace *> char '[' *> int)
                    )
            )

{- a repeat section at the start of a bar.  We have just parsed a bar marker (say |) and so the combination of this and the repeat may be:
      |1
      |[1
      | [1
   but is not allowed to be
      | 1

   associating the digit with the bracket bar number should remove ambiguity with respect to other productions that use the bracket
   (in particular, inline headers and chords).
-}
repeatSection :: Parser Int
repeatSection =
   int

{- JMW FIX ME
    choice
        [
          (whiteSpace *> char '[' *> int)
        , int
        ]
-}


{- written like this instead of a regex because it's all regex control character! -}
barSeparator :: Parser String
barSeparator =
      choice
          [ string "[|"
          , string "|]:"  -- must come before |] otherwise it hides it
          , string "|]"
          , string "]|:"
          , string "]|"
          , string ":[|"
          , string "|:"
          , string ":|:"
          , string ":||:"
          , string ":|]"   -- must come before :| else it hides it
          , string ":||"
          , string ":|"
          , string "::"
          , string "||:"   -- must come before || else it hides it
          , string "||"
          , string "|"     -- must be last otherwise it hides |:
          ]

{-
      choice
          [ string "|"     -- must be last otherwise it hides |:
          , string "||"
          , string "||:"   -- must come before || else it hides it
          , string "::"
          , string ":|"
          , string ":||"
          , string ":|]"   -- must come before :| else it hides it
          , string ":||:"
          , string "|:"
          , string ":[|"
          , string "]|"
          , string "]|:"
          , string "|]"
          , string "|]:"  -- must come before |] otherwise it hides it
          , string "[|"
          ]
  -}



-- spec is unclear if spaces are allowed after a broken rhythm operator but it's easy to support, is more permissive and doesn't break anything


brokenRhythmTie :: Parser Broken
brokenRhythmTie =
    buildBrokenOperator <$> regex "(<+|>+)" <* whiteSpace


brokenRhythmPair :: Parser Music
brokenRhythmPair =
  traceParse "brokenRhythmPair" <$>
    (
    BrokenRhythmPair
        <$> abcNote
        <*> brokenRhythmTie
        <*> abcNote
        <?> "broken rhythm pair"
    )


note :: Parser Music
note =
  traceParse "note" <$>
    (
      Note <$> abcNote
    )


abcNote :: Parser AbcNote
abcNote =
  traceParse "abcNote" <$>
    (
    buildNote
        <$> maybeAccidental
        <*> pitch
        <*> moveOctave
        <*> optionMaybe noteDur
        <*> maybeTie
        <?> "ABC note"
    )

{- maybe an accidental defining a note's pitch -}
maybeAccidental :: Parser (Maybe Accidental)
maybeAccidental =
    optionMaybe accidental

accidental :: Parser Accidental
accidental =
    buildAccidental
        <$> (choice
                [
                  string "^^"
                , string "__"
                , string "^"
                , string "_"
                , string "="
                ]
            )

{- an upper or lower case note ([A-Ga-g]) -}
pitch :: Parser String
pitch =
    regex "[A-Ga-g]"

moveOctave :: Parser Int
moveOctave =
    octaveShift <$> regex "[',]*"


{- count the number of apostrophe (up) or comma (down) characters in the string
   and give the result a value of (up-down)
-}
octaveShift :: String -> Int
octaveShift s =
    let
      up = length $ filter ( (==) '\'') s
      down = length $ filter ( (==) ',') s
    in
      up - down

{- the duration of a note in the body
   order of choices here is important to remove ambiguity
-}
noteDur :: Parser Rational
noteDur =
    choice
      [
        try twoSlashes
      , try anyRat
      , integralAsRational
      ]

{-}
        [ slashesRational          -- e.g. / or //
        , curtailedLeftRational    -- e.g. /2
        , integralAsRational
        , integralAsRational       -- e.g. 3
        , curtailedRightRational   -- e.g. 3/
        , rational                 -- e.g. 3/2
        ]
        -}


{-
    choice
        [ rational                  -- e.g. 3/2
        , curtailedRightRational    -- e.g. 3/
        , integralAsRational        -- e.g. 3
        , curtailedLeftRational     -- e.g. /2
        , slashesRational           -- e.g. / or //
        ]
-}

anyRat :: Parser Rational
anyRat =
  Rational.rational <$> option 1 int <* char '/' <*> option 2 int

twoSlashes :: Parser Rational
twoSlashes =
  traceParse "twoSlashes" <$>
    (
    Rational.rational 1 4 <$ char '/' <* char '/'
    )

{- normal Rational e.g 3/4 -}
rational :: Parser Rational
rational =
  traceParse "rational" <$>
    (
    Rational.rational <$> int <* char '/' <*> int
    )

{- e.g. /4 (as found in note durations) -}
curtailedLeftRational :: Parser Rational
curtailedLeftRational =
  traceParse "curtailed left rational" <$>
    (
    (Rational.rational 1) <$> (char '/' *> int)
    )

{- e.g. 3/ (as found in note durations) -}
curtailedRightRational :: Parser Rational
curtailedRightRational =
  traceParse "curtailed right rational" <$>
    (
    invert <$> (Rational.rational 2 <$> (int <* char '/'))
    )

integralAsRational :: Parser Rational
integralAsRational =
  traceParse "integralAsRational" <$>
    (
    fromInt <$> int
    )

{- e.g. / or // or /// (as found in note durations)
   which translates to 1/2, 1/4, 1/8 etc
-}
slashesRational :: Parser Rational
slashesRational =
  traceParse "slashes rational" <$>
    (
    buildRationalFromExponential <$> List.length <$> many1 (char '/')
    )

{- attaches to leading barand not free-standing -}
maybeTie :: Parser (Maybe Char)
maybeTie =
    (optionMaybe (char '-'))
        <?> "tie"

rest :: Parser Music
rest =
  traceParse "rest" <$>
    (
    Rest
        <$> (fromMaybe (fromInt 1) <$> (regex "[XxZz]" *> optionMaybe noteDur))
        <?> "rest"
    )

tuplet :: Parser Music
tuplet =
    Tuplet
        <$> (char '(' *> tupletSignature)
        <*> many1 abcNote
        <?> "tuplet"

{- possible tuplet signatures
   (3             --> {3,2,3}
   (3:2           --> {3,2,3}
   (3::           --> {3,2,3}
   (3:2:4         --> {3,2,4}
   (3::2          --> {3,2,2}

   note, space is allowed after the tuplet signature but before the notes in the tuplet
-}
tupletSignature :: Parser TupletSignature
tupletSignature =
    buildTupletSignature
        <$> regex "[2-9]"
        <*> tup
        <*> tup
        <* whiteSpace

tup :: Parser (Maybe String)
tup =
    join
        <$> optionMaybe
                (char ':' *> optionMaybe (regex "[2-9]"))

{- Note, Slur should really be defined as Slur (List Music) and then parsed as shown below.  This would allow slurs to be
      nested and the parser to test that the brackets are balanced.  However, unfortunately, in the wild there are examples
      of slurs that go across music lines which make this interpretation impossible.  We thus simply parse the bracket characters.


   slur :: Parser Music
   slur = fix \i ->
           Slur <$>  parens (many1 musicItem)
                <?> "slur"
-}
slur :: Parser Music
slur =
    Slur
        <$> (char '(' <|> char ')')
        <?> "slur"

graceNote :: Parser Music
graceNote =
    between (char '{') (char '}') grace
        <?> "grace note"

grace :: Parser Music
grace =
    GraceNote <$> acciaccatura <*> (many1 abcNote)

{- acciaccaturas are indicated with an optional forward slash
   was
    acciaccatura = withDefault false <$> ( (\_ -> true) <$> maybe (char '/'))
-}
acciaccatura :: Parser Boolean
acciaccatura =
    (\_ -> true) <$> optionMaybe (char '/')

{- an annotation to the score
   4.19 Annotations

   General text annotations can be added above, below or on the staff in a similar way to chord symbols. In this case, the string within double quotes
   is preceded by one of five symbols ^, _, <, > or @ which controls where the annotation is to be placed; above, below, to the left or right respectively
   of the following note, rest or bar line. Using the @ symbol leaves the exact placing of the string to the discretion of the interpreting program.
   These placement specifiers distinguish annotations from chord symbols, and should prevent programs from attempting to play or transpose them.
   All text that follows the placement specifier is treated as a text string.

   Example:

   "<(" ">)" C

-}
annotation :: Parser Music
annotation =
    buildAnnotation
        <$> annotationString
        <?> "annotation"

annotationString :: Parser String
annotationString =
    string "\""
        *> regex "[\\^\\>\\<-@](\\\\\"|[^\"\n])*"
        <* string "\""
        <?> "annotation"

{- a free - format chord symbol - see 4.18 Chord symbols -}
chordSymbol :: Parser Music
chordSymbol =
    ChordSymbol
        <$> quotedString
        <?> "chord symbol"

decoration :: Parser Music
decoration =
    Decoration
        <$> (shortDecoration <|> longDecoration)
        <?> "decoration"


shortDecoration :: Parser String
shortDecoration =
    regex "[\\.~HLMOPSTuv]"
        <?> "short decoration"


longDecoration :: Parser String
longDecoration =
    between (char '!') (char '!') (regex "[^\x0D\n!]*")
        <?> "long decoration"

{-| our whiteSpace differs from that of the string parser we do NOT want to
  consume carriage returns or newlines
-}

whiteSpace :: Parser String
whiteSpace =
  foldMap singleton <$>
     many
       (choice
         [ space
         , tab
         ]
        )

-- at least one (intended) space somewhere inside the music body
spacer :: Parser Music
spacer =
  traceParse "spacer" <$>
    (
    Spacer
        <$> (List.length <$> (many1 scoreSpace))
        <?> "space"
    )


{- space within a line of the tune's score -}
scoreSpace :: Parser Char
scoreSpace =
    choice
        [
          tab
        , char 'y'
        , space
        ]

space :: Parser Char
space = char ' '

tab :: Parser Char
tab = char '\t'

{- characters to ignore

   Section 8.1 Tune Body:

   The following characters are currently reserved: # * ; ? @
   In future standards they may be used to extend the abc syntax. To ensure forward compatibility,
   current software should ignore these characters when they appear inside or between note groups.

   section 4.7 Beams:

   Back quotes ` may be used freely between notes to be beamed, to increase legibility.
   They are ignored by computer programs. For example, A2``B``C is equivalent to A2BC.
-}
ignore :: Parser Music
ignore =
    Ignore <$
        (regex "[#@;`\\*\\?]+")
        <?> "ignored character"

{- this is an area where the spec is uncertain.  See 6.1.1 Typesetting line-breaks
   The forward slash is used to indicate 'continuation of input lines' often because
   users may need to avoid long lines if, for example, they would otherwise extend
   beyond the limit of an old email system.  All very out of date, but nevertheless
   still prevalent in the wild.  We take the view that we must do our best to recognise
   them and then throw them away (along with any other later stuff in the line)

   Return Continuation if we have a continuation
-}
continuation :: Parser Music
continuation =
    Continuation
        <$ char '\\'
        <* regex "[^\x0D\n]*"
        <?> "continuation"

-- tune headers

headers :: Parser TuneHeaders
headers =
    many header <?> "headers"

header :: Parser Header
header =
  traceParse "header" <$>
    (
    informationField false <* eol
    )

{- headers that may appear in the tune body -}
tuneBodyHeader :: Parser BodyPart
tuneBodyHeader =
    BodyInfo
        <$> tuneBodyInfo true
        <* eol
        <?> "tune body header"

tuneBodyInfo :: Boolean -> Parser Header
tuneBodyInfo isInline =
    choice
        [
          tuneBodyOnlyInfo isInline
        , anywhereInfo isInline
        ]
        <?> "tune body info"

tuneBodyOnlyInfo :: Boolean -> Parser Header
tuneBodyOnlyInfo isInline =
    choice
        [
          symbolLine isInline
        , wordsAligned isInline
        ]
        <?> "tune body only info"



{- Headers/Information fields.  These can be used in three different ways:
     1) As a normal tune header
     2) As an 'inline' header inside the tune body on a separate line
     3) Embedded inside a tune score between '[' and ']'

   Only a named subset of headers can be used inline in this way.

   One subtlety is therefore that header information that accepts simple text content
   should not be allowed to incorporate '[' or ']' because of the potential ambiguity.
   Thus, headers functions are given a parameter 'inline' which is the inline context
   simply allowing 'normal' headers to accept these values in text content but to allow
   inline headers to reject them.
-}
{- whereas information fields can be used inline
   isInline - is this information field being used in an in-line context
   (as opposed to being used in a conventional header)
-}
informationField :: Boolean -> Parser Header
informationField isInline =
  traceParse "informationField" <$>
    (choice
        [
          anywhereInfo isInline
        , tuneInfo
        ]
        <?> "header"
    )

anywhereInfo :: Boolean -> Parser Header
anywhereInfo isInline =
  traceParse "anywhereInfo" <$>
    (
    choice
        [ instruction isInline
        , key
        , unitNoteLength
        , meter
        , macro isInline
        , notes isInline
        , parts isInline
        , tempo
        , rhythm isInline
        , remark isInline
        , title isInline
        , userDefined isInline
        , voice isInline
        , wordsAfter isInline
        , fieldContinuation
        , comment
        ]
        <?> "anywhere info"
    )

tuneInfo :: Parser Header
tuneInfo =
    choice
        [
          area
        , book
        , composer
        , discography
        , fileUrl
        , group
        , history
        , origin
        , source
        , referenceNumber
        , transcription
        , unsupportedHeader  -- headers that are currently unsupported but must be recognized and ignored
        ]
        <?> "tune info"


headerCode :: Char -> Parser Char
headerCode c =
    char c <* char ':' <* whiteSpace


unsupportedHeaderCode :: Parser String
unsupportedHeaderCode =
    regex "[a-qt-vx-zEJ]" <* char ':' <* whiteSpace

{- comments.  These are introduced with '%' and can occur anywhere.
   The stylesheet directive '%%' is not recognized here and will
   simply be treated as a comment.  We'll treat comments as Headers
   so as not to pollute the parse tree overmuch
-}
comment :: Parser Header
comment =
    Comment
        <$> (regex "%" *> strToEol)
        <?> "comment"

{- parse an information item String - note that, because these can be used inline
   (bracketed by '[' and ']') it behoves us not to use the framing characters in the string
   when the header is used inline (but not when used in a normal header)
   not that the spec has anything to say about it as far as I can see
-}
inlineInfo :: Boolean -> Parser String
inlineInfo isInline =
    let
        pattern =
            if isInline then
                "[^\x0D\n\\[\\]]*"
            else
                "[^\x0D\n]*"
    in
        regex pattern

area :: Parser Header
area =
    Area
        <$> ((headerCode 'A') *> strToEol)
        <?> "A header"


book :: Parser Header
book =
    Book
        <$> ((headerCode 'B') *> strToEol)
        <?> "B Header"


composer :: Parser Header
composer =
    Composer
        <$> ((headerCode 'C') *> strToEol)
        <?> "C header"


discography :: Parser Header
discography =
    Discography
        <$> ((headerCode 'D') *> strToEol)
        <?> "D header"


fileUrl :: Parser Header
fileUrl =
    FileUrl
        <$> ((headerCode 'F') *> strToEol)
        <?> "F header"


group :: Parser Header
group =
    Group
        <$> ((headerCode 'G') *> strToEol)
        <?> "G header"


history :: Parser Header
history =
    History
        <$> ((headerCode 'H') *> strToEol)
        <?> "H header"


instruction :: Boolean -> Parser Header
instruction isInline =
    Instruction
        <$> ((headerCode 'I') *> (inlineInfo isInline))
        <?> "I header"


key :: Parser Header
key =
    buildKey
        <$> (headerCode 'K')
        <*> keySignature
        <*> keyAccidentals
        <* whiteSpace
        <?> "K header"


unitNoteLength :: Parser Header
unitNoteLength =
    UnitNoteLength
        <$> ((headerCode 'L') *> noteDuration)
        <?> "L header"


meter :: Parser Header
meter =
    Meter
        <$> ((headerCode 'M') *> meterDefinition)
        <?> "M header"


macro :: Boolean -> Parser Header
macro isInline =
    Macro
        <$> ((headerCode 'm') *> (inlineInfo isInline))
        <?> "m header"


notes :: Boolean -> Parser Header
notes isInline =
    Notes
        <$> ((headerCode 'N') *> (inlineInfo isInline))
        <?> "N header"


origin :: Parser Header
origin =
    Origin
        <$> ((headerCode 'O') *> strToEol)
        <?> "O header"


parts :: Boolean -> Parser Header
parts isInline =
    Parts
        <$> ((headerCode 'P') *> (inlineInfo isInline))
        <?> "P header"


tempo :: Parser Header
tempo =
    Tempo
        <$> ((headerCode 'Q') *> tempoSignature)
        <?> "Q header"


rhythm :: Boolean -> Parser Header
rhythm isInline =
    Rhythm
        <$> ((headerCode 'R') *> (inlineInfo isInline))
        <?> "R header"


remark :: Boolean -> Parser Header
remark isInline =
    Remark
        <$> ((headerCode 'r') *> (inlineInfo isInline))
        <?> "r header"


source :: Parser Header
source =
    Source
        <$> ((headerCode 'S') *> strToEol)
        <?> "S header"


symbolLine :: Boolean -> Parser Header
symbolLine isInline =
    SymbolLine
        <$> ((headerCode 's') *> (inlineInfo isInline))
        <?> "s header"


title :: Boolean -> Parser Header
title isInline =
  traceParse "title" <$>
    (
    Title
        <$> ((headerCode 'T') *> (inlineInfo isInline))
        <?> "T header"
    )


userDefined :: Boolean -> Parser Header
userDefined isInline =
    UserDefined
        <$> ((headerCode 'U') *> (inlineInfo isInline))
        <?> "U header"


voice :: Boolean -> Parser Header
voice isInline =
    Voice
        <$> ((headerCode 'V') *> (inlineInfo isInline))
        <?> "V header"


wordsAfter :: Boolean -> Parser Header
wordsAfter isInline =
    WordsAfter
        <$> ((headerCode 'W') *> (inlineInfo isInline))
        <?> "W header"

wordsAligned :: Boolean -> Parser Header
wordsAligned isInline =
    WordsAligned
        <$> ((headerCode 'w') *> (inlineInfo isInline))
        <?> "w header"

referenceNumber :: Parser Header
referenceNumber =
    ReferenceNumber
        <$> ((headerCode 'X') *> int)
        <?> "x header"

transcription :: Parser Header
transcription =
    Transcription
        <$> ((headerCode 'Z') *> strToEol)
        <?> "Z header"

fieldContinuation :: Parser Header
fieldContinuation =
    FieldContinuation
        <$> ((headerCode '+') *> strToEol)
        <?> "field continuation"

{- unsupported header reserved for future use -}
unsupportedHeader :: Parser Header
unsupportedHeader =
    UnsupportedHeader
        <$ unsupportedHeaderCode
        <* strToEol
        <?> "unsupported header"


-- HEADER ATTRIBUTES
-- rational with trailing optional spaces
headerRational :: Parser Rational
headerRational =
    rational <* whiteSpace


noteDuration :: Parser NoteDuration
noteDuration =
    rational <* whiteSpace

meterDefinition :: Parser (Maybe MeterSignature)
meterDefinition =
    choice
        [
          cutTime
        , commonTime
        , meterSignature
        , nometer
        ]


commonTime :: Parser (Maybe MeterSignature)
commonTime =
    (Just (Tuple 4 4 )) <$ char 'C'


cutTime :: Parser (Maybe MeterSignature)
cutTime =
    (Just (Tuple 2 2 )) <$ string "C|"



-- can't use Rationals for these because they cancel
meterSignature :: Parser (Maybe MeterSignature)
meterSignature =
    Just <$> (Tuple <$> int <* char '/' <*> int <* whiteSpace)


nometer :: Parser (Maybe MeterSignature)
nometer =
    Nothing <$ string "none"

tempoSignature :: Parser TempoSignature
tempoSignature =
    buildTempoSignature <$> optionMaybe spacedQuotedString <*> many headerRational <*> optionMaybe (char '=') <*> int <*> optionMaybe spacedQuotedString <* whiteSpace


sharpOrFlat :: Parser Accidental
sharpOrFlat =
    map
        (\x ->
            if x == '#' then
                Sharp
            else
                Flat
        )
        (char '#' <|> char 'b' )

keyName :: Parser String
keyName =
    regex "[A-G]"

keySignature :: Parser KeySignature
keySignature =
    buildKeySignature <$> keyName <*> optionMaybe sharpOrFlat <*> optionMaybe mode

{- a complete list of key accidentals which may be empty -}
keyAccidentals :: Parser KeySet
keyAccidentals =
    buildKeyAccidentals <$> spacelessAccidental <*> keyAccidentalsList

{- I think the first in the list is optionally introduced without a space  (judging by what's in the wild) -}
spacelessAccidental :: Parser (Maybe KeyAccidental)
spacelessAccidental =
    optionMaybe keyAccidental

{- there may be zero or more key accidentals, separated by spaces (KeySet is a List of Key Accidentals) -}
keyAccidentalsList :: Parser KeySet
keyAccidentalsList =
    many (space *> keyAccidental)

{- a key accidental as an amendment to a key signature - as in e.g. K:D Phr ^f -}
keyAccidental :: Parser KeyAccidental
keyAccidental =
    buildKeyAccidental <$> accidental <*> pitch

mode :: Parser Mode
mode =
    choice
        [
          try major
        , ionian
        , dorian
        , phrygian
        , lydian
        , mixolydian
        , aeolian
        , locrian
        , minor -- place 'last' because of potential ambiguitymajor
        ]


minor :: Parser Mode
minor =
    Minor <$ whiteSpace <* regex "[M|m][A-Za-z]*"


major :: Parser Mode
major =
    Major <$ whiteSpace <* regex "[M|m][A|a][J|j][A-Za-z]*"


ionian :: Parser Mode
ionian =
    -- Ionian <$ whiteSpace <* regex "(I|i)(O|o)(N|n)([A-Za-z])*"
    Ionian <$ whiteSpace <* regex "[I|i][O|o][N|n][A-Za-z]*"


dorian :: Parser Mode
dorian =
    Dorian <$ whiteSpace <* regex "[D|d][O|o][R|r][A-Za-z]*"


phrygian :: Parser Mode
phrygian =
    Phrygian <$ whiteSpace <* regex "[P|p][H|h][R|r][A-Za-z]*"


lydian :: Parser Mode
lydian =
    Lydian <$ whiteSpace <* regex "[L|l][Y|y][D|d][A-Za-z]*"


mixolydian :: Parser Mode
mixolydian =
    Mixolydian <$ whiteSpace <* regex "[M|m][I|i][X|x][A-Za-z]*"


aeolian :: Parser Mode
aeolian =
    Aeolian <$ whiteSpace <* regex "[A|a][E|e][O|o][A-Za-z]*"


locrian :: Parser Mode
locrian =
    Locrian <$ whiteSpace <* regex "[L|l][O|o][C|c][A-Za-z]*"



-- builders
buildAbcTune :: TuneHeaders -> TuneBody -> AbcTune
buildAbcTune hs b =
  { headers : hs, body : b}

buildBar :: Thickness -> Maybe Repeat -> Maybe Int -> Bar
buildBar t r i =
  { thickness : t, repeat : r, iteration : i}

{- build a bar line
   this is a bit tricky because of the poor specification for the possible shapes of bar lines
   which may have multiple different types of bar line markers (|,[,]) and repeat markers (:)
   Try to normalise to representations of basic shapes like (|, |:, :|, :||, ||:, ||, :|:, :||: )

-}
buildBarline :: String -> Maybe Int -> Music
buildBarline s i =
    let
        -- estimate the bar separator thickness
        thickness =
            if (includes "|]" s) then
                ThinThick
            else if (includes "[|" s) then
                ThickThin
            else if (includes "||" s) then
                ThinThin
            else
                Thin

        -- now normalise all lines to '|'
        f c =
            case c of
                '[' ->
                    '|'

                ']' ->
                    '|'

                _ ->
                    c

        normalised =
            mapChars f s

        -- count the repeat markers
        repeatCount =
            length (filter (\c -> c == ':') normalised)

        -- set the repeat
        repeat =
            if (repeatCount == 0) then
                Nothing
            else if (repeatCount == 1) then
                if includes ":|" normalised then
                    Just End
                else
                    Just Begin
            else
                Just BeginAndEnd
    in
        Barline { thickness : thickness, repeat : repeat, iteration : i }

buildBrokenOperator :: String -> Broken
buildBrokenOperator s =
    if startsWith "<" s then
        LeftArrow (length s)
    else
        RightArrow (length s)

buildNote :: Maybe Accidental -> String -> Int -> Maybe Rational -> Maybe Char -> AbcNote
buildNote macc pitchStr octave ml mt =
    let
        l =
            fromMaybe (1 % 1) ml

        -- a = buildAccidental macc
        p =
            lookupPitch (toUpper pitchStr)

        spn =
            scientificPitchNotation pitchStr octave

        tied =
            case mt of
                Just _ ->
                    true

                _ ->
                    false
    in
        { pitchClass : p, accidental : macc, octave : spn, duration : l, tied : tied }


buildAccidental :: String -> Accidental
buildAccidental s =
    case s of
        "^^" ->
            DoubleSharp

        "__" ->
            DoubleFlat

        "^" ->
            Sharp

        "_" ->
            Flat

        _ ->
            Natural

buildKeyAccidental :: Accidental -> String -> KeyAccidental
buildKeyAccidental a pitchStr =
    let
        pc =
            lookupPitch pitchStr
    in
        Tuple pc a


buildKeyAccidentals :: Maybe KeyAccidental -> List KeyAccidental -> List KeyAccidental
buildKeyAccidentals mac acs =
    case mac of
        Just ac ->
            ac : acs
        _ ->
            acs

buildChord :: List AbcNote -> Maybe Rational -> AbcChord
buildChord ns ml =
    let
        l =
            fromMaybe (fromInt 1) ml
    in
        { notes : ns, duration : l }

{- investigate a note/octave pair and return the octave
   in scientific pitch notation relative to MIDI pitches
   CHANGE THIS
-}
scientificPitchNotation :: String -> Int -> Int
scientificPitchNotation pc oct =
    if includes pc "ABCDEFG" then
        -- pitch class inhabits octave of middle C, oct <= 0
        middlecOctave + oct
    else
        -- pitch class inhabits octave above middle C, oct >= 0
        middlecOctave + 1 + oct

{- used in counting slashes exponentially -}


buildRationalFromExponential :: Int -> Rational
buildRationalFromExponential i =
    (1 % (pow 2 i))

{- build a tuplet signature {p,q,r) - p notes in the time taken for q
   in operation over the next r notes
-}
buildTupletSignature :: String -> Maybe String -> Maybe String -> TupletSignature
buildTupletSignature ps mq mr =
    let
        p =
            toTupletInt ps

        -- default values for q.  Not quite in accordance with spec where q varies
        -- between 2 and 3 for odd values of p, dependent on the time signature
        -- (but this would make the parser stateful which we don't want for such small
        -- edge cases)
        qdefault =
            case p of
                2 ->
                    3

                3 ->
                    2

                4 ->
                    3

                6 ->
                    2

                8 ->
                    3

                _ ->
                    2

        q =
            fromMaybe qdefault (map toTupletInt mq)

        r =
            fromMaybe p (map toTupletInt mr)
    in
        { p : p, q : q, r : r }

toTupletInt :: String -> Int
toTupletInt s =
    fromMaybe 3 (fromString s)

buildAnnotation :: String -> Music
buildAnnotation s =
    let
        firstChar =
            charAt 0 s

        placement =
            case firstChar of
                Just '^' ->
                    AboveNextSymbol

                Just '_' ->
                    BelowNextSymbol

                Just '<' ->
                    LeftOfNextSymbol

                Just '>' ->
                    RightOfNextSymbol

                _ ->
                    Discretional
    in
        Annotation placement s

buildTempoSignature :: Maybe String -> List Rational -> Maybe Char -> Int -> Maybe String -> TempoSignature
buildTempoSignature ms1 fs c i ms2 =
    let
        ms =
            case ms1 of
                Nothing ->
                    ms2

                _ ->
                    ms1
        noteLengths =
          case fs of
            Nil ->
                List.singleton (1 % 4)
            _ ->
                fs
    in
        { noteLengths : noteLengths
        , bpm : i
        , marking : ms
        }


{- build a key signature -}
buildKeySignature :: String -> Maybe Accidental -> Maybe Mode -> KeySignature
buildKeySignature pStr ma mm =
    { pitchClass : lookupPitch pStr, accidental : ma, mode : fromMaybe Major mm }

{- build a complete key designation (key signature plus modifying accidentals) -}
buildKey :: Char -> KeySignature -> List KeyAccidental -> Header
buildKey c ks ka =
    Key ( Tuple ks ka )

-- lookups

lookupPitch :: String -> PitchClass
lookupPitch p =
   case (toUpper p) of
     "A" -> A
     "B" -> B
     "C" -> C
     "D" -> D
     "E" -> E
     "F" -> F
     "G" -> G
     _ -> C

-- low level

{-| Parse a `\n` character. -}
newline :: Parser Char
newline = satisfy ((==) '\n') <?> "expected newline"


{-| Parse a `\r\n` sequence, returning a `\n` character. -}
crlf :: Parser Char
crlf = '\n' <$ regex "\r\n" <?> "expected crlf"


{-| Parse an end of line character or sequence, returning a `\n` character. -}
eol :: Parser Char
eol = newline <|> crlf

{- parse a remaining string up to but not including the end of line
   was
      strToEol = String.fromList <$> many (noneOf [ '\r', '\n' ])
-}
strToEol :: Parser String
strToEol =
    regex "[^\x0D\n]*"



manyTill1 :: forall a end. Parser a -> Parser end -> Parser (List a)
manyTill1 = manyTill

{-| Parse a positive integer (with no sign).
-}
int :: Parser Int
int =
  fromMaybe 1 <$>  -- the regex will always provide an integer if it parses
    fromString <$>
    regex "(0|[1-9][0-9]*)"
    <?> "expected a positive integer"

quotedString :: Parser String
quotedString =
    string "\""
       *> regex "(\\\\\"|[^\"\n])*"
       <* string "\""
       <?> "quoted string"

spacedQuotedString :: Parser String
spacedQuotedString =
    try -- whitespace can crop up anywhere
      (whiteSpace *> quotedString <* whiteSpace)


-- utility Functions
concatenate :: List String -> String
concatenate = foldr (<>) ""

invert :: Rational -> Rational
invert r =
  -- (denominator r % numerator r)
  (1 % 1) / r



{-| Entry point - Parse an ABC tune image.
-}
parse :: String -> Either ParseError AbcTune
parse s =
    case runParser abc s of
        Right n ->
            Right n

        Left e ->
            Left e


parseKeySignature :: String -> Either ParseError ModifiedKeySignature
parseKeySignature s =
    case runParser keySignature s of
        Right n ->
          let
             emptyList = Nil :: List KeyAccidental
          in
             Right (Tuple n emptyList)

        Left e ->
            Left e

-- | An ABC Parser.
module Data.Abc.Parser
        ( PositionedParseError(..)
        , parse
        , parseKeySignature
        ) where

import Data.Abc

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Foldable (foldr, foldMap)
import Data.Functor (map)
import Data.Int (fromString, pow)
import Data.List (List(..), (:))
import Data.List.NonEmpty as Nel
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Rational (Rational, fromInt, (%))
import Data.String (toUpper, singleton)
import Data.String.CodePoints (codePointFromChar, length)
import Data.String.CodeUnits (charAt, fromCharArray, toCharArray)
import Data.String.Utils (startsWith, includes)
import Data.Tuple (Tuple(..))
import Data.Map (Map)
import Data.Map (fromFoldable) as Map
import Prelude (class Show, flip, join, show, ($), (*>), (+), (-), (/), (<$), (<$>), (<*), (<*>), (<<<), (<>), (==))
import Text.Parsing.StringParser (Parser(..), ParseError(..), Pos, try)
import Text.Parsing.StringParser.Combinators (between, choice, many, many1, manyTill, option, optionMaybe, sepBy, (<?>))
import Text.Parsing.StringParser.String (satisfy, string, alphaNum, char, eof, regex)
-- import Debug.Trace (trace)


{- transient data type just used for parsing the awkward Tempo syntax
  a list of time signatures expressed as rationals and a bpm expressed as an Int
-}
data TempoDesignation = TempoDesignation (Nel.NonEmptyList Rational) Int

{- use for debug like this:

   traceParse "manySlashes" <$>
    (
      parseRule
    )
-}
{-
traceParse :: forall a. String -> a -> a
traceParse s p =
  trace s (\_ -> p)
-}

-- | a parse error and its accompanying position in the text
newtype PositionedParseError = PositionedParseError
  { pos :: Int
  , error :: String
  }

instance showKeyPositionedParseError :: Show PositionedParseError where
  show (PositionedParseError e) = e.error <> " at position " <> show e.pos

abc :: Parser AbcTune
abc =
    buildAbcTune <$> headers <*> body

body :: Parser (List BodyPart)
body =
    (:)
        <$> score
        <*> manyTill
                {- there is unfortunately ambiguity between a score item and
                   an in-score header.  For example 'M' may introduce both a
                   decoration or a meter.  We probably don't pay too much for
                   trying the header because they are only short sequences
                -}
                (try tuneBodyHeader <|> score)
                eof

score :: Parser BodyPart
score =
    Score <$>
      {- there is potential ambiguity here betweeb 'fullyBarredLine' and
         'inline' both of which commence with '[' making this order in the
         pairing necessary.  To do it in the other order would involve:
           (try fullyBarredLine <|> introLine)
      -}
      ( introLine <|> fullyBarredLine )
       <?> "score"

bar :: Parser Bar
bar =
  buildBar <$> barline <*> (many scoreItem)
   <?> "bar"

-- | an intro bar is a bar at the beginning of a line which has no starting bar line
introBar :: Parser Bar
introBar =
  buildBar invisibleBarType <$> many scoreItem
   <?> "intro bar"

-- | an intro line as a full line of bars thus introduced
introLine :: Parser (List Bar)
introLine =
  (:) <$> introBar <*> manyTill1 bar eol
   <?> "intro line"

-- | a fully barred line has bar lines both at begin and end
fullyBarredLine :: Parser (List Bar)
fullyBarredLine =
  manyTill1 bar eol
   <?> "fully barred line"

-- | and an intoLine is a line of bars started by an intro

scoreItem :: Parser Music
scoreItem =
  choice
    [
      try chord -- potential ambiguity with (inline) in-score headers
    , try inline
    , continuation
    , decoratedSpace  -- potential ambiguity with a decorated note
    , ignore
    , spacer
    , chordSymbol
    , annotation
    , try tuplet  -- potential ambiguity with slurs
    , slur
    , rest
    , try brokenRhythmPair -- potential ambiguity with note
    , note
    ]
      <?> "score item"

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

barline :: Parser BarType
barline =
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
normalBarline :: Parser BarType
normalBarline =
    buildBarline
        <$> barSeparator
        <*> optionMaybe repeatSection
        <?> "bartype"

{- sometimes in the wild we get a degenerate repeat marker at the start of a line of music like this:
     [1 .....
   or
     _[1 ....
   again we have to be careful about ambiguity between this and inline headers by making sure we parse '[' immediately followed by '1' etc.
-}
degenerateBarRepeat :: Parser BarType
degenerateBarRepeat =
  (buildBarTypeRecord Thin Nothing
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

-- spec is unclear if spaces are allowed after a broken rhythm operator but it's easy to support, is more permissive and doesn't break anything
brokenRhythmTie :: Parser Broken
brokenRhythmTie =
    buildBrokenOperator <$> brokenRhythmOperator <* whiteSpace

brokenRhythmPair :: Parser Music
brokenRhythmPair =
    BrokenRhythmPair
        <$> graceableNote
        <*> brokenRhythmTie
        <*> graceableNote
        <?> "broken rhythm pair"

note :: Parser Music
note =
  Note <$> graceableNote

abcNote :: Parser AbcNote
abcNote =
    buildNote
        <$> maybeAccidental
        <*> pitch
        <*> moveOctave
        <*> optionMaybe noteDur
        <*> maybeTie
        <?> "ABC note"

graceableNote :: Parser GraceableNote
graceableNote =
  buildGraceableNote
    <$> optionMaybe graceBracket
    <*> decorations
    <*> abcNote
    <?> "graceable note"

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
      up = Array.length $ Array.filter ( (==) '\'') (toCharArray s)
      down = Array.length $ Array.filter ( (==) ',') (toCharArray s)
    in
      up - down

{- the duration of a note in the body
   order of choices here is important to remove ambiguity
-}
noteDur :: Parser Rational
noteDur =
    choice
      [
        try manySlashes
      , try anyRat
      , integralAsRational
      ]

{-| this matches:
       1/2
       /2
       1
       /
   i.e. there has to be at least a single slash
-}
anyRat :: Parser Rational
anyRat =
  (%) <$> option 1 int <* char '/' <*> option 2 int

{-| this matches //  or /// etc. -}
manySlashes :: Parser Rational
manySlashes =
    buildRationalFromSlashList
      <$> (Nel.cons <$> char '/' <*> many1 (char '/'))


integralAsRational :: Parser Rational
integralAsRational =
    fromInt <$> int

{- attaches to leading barand not free-standing -}
maybeTie :: Parser (Maybe Char)
maybeTie =
    (optionMaybe (char '-'))
        <?> "tie"

rest :: Parser Music
rest =
  Rest
    <$> abcRest
    <?> "rest"

abcRest :: Parser AbcRest
abcRest =
  buildRest
      <$> (fromMaybe (fromInt 1) <$> (regex "[XxZz]" *> optionMaybe noteDur))
      <?> "abcRest"

-- | tuplets may now contain either a (Left) rest or a (Right) Note
restOrNote :: Parser RestOrNote
restOrNote =
  (Left <$> abcRest) <|> (Right <$> graceableNote)

tuplet :: Parser Music
tuplet =
    Tuplet
        <$> (char '(' *> tupletSignature)
        <*> many1 restOrNote
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
        <$> tupletLength
        <*> tup
        <*> tup
        <* whiteSpace

tup :: Parser (Maybe String)
tup =
    join
        <$> optionMaybe
                (char ':' *> optionMaybe tupletLength)

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

graceBracket :: Parser Grace
graceBracket =
    between (char '{') (char '}') grace
        <?> "grace bracket"

grace :: Parser Grace
grace =
    buildGrace <$> acciaccatura <*> (many1 abcNote)

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

decorations :: Parser (List String)
decorations =
  many decoration

decoration :: Parser String
decoration =
   (shortDecoration <|> longDecoration)
     <* whiteSpace
      <?> "decoration"

shortDecoration :: Parser String
shortDecoration =
    regex "[\\.~HLMOPSTuv]"
        <?> "short decoration"

longDecoration :: Parser String
longDecoration =
    between (char '!') (char '!') (regex "[^\x0D\n!]*")
        <?> "long decoration"

-- | our whiteSpace differs from that of the string parser we do NOT want to
-- |consume carriage returns or newlines
whiteSpace :: Parser String
whiteSpace =
  foldMap (singleton <<< codePointFromChar ) <$>
     many scoreSpace

-- at least one (intended) space somewhere inside the music body
spacer :: Parser Music
spacer =
    Spacer
        <$> (Nel.length <$> (many1 scoreSpace))
        <?> "space"

-- | see section 6.1.2 Typesetting extra space
-- | y can be used to add extra space between the surrounding notes; moreover,
-- | chord symbols and decorations can be attached to it, to separate them from notes.
decoratedSpace :: Parser Music
decoratedSpace =
  try (DecoratedSpace <$> decorations <* char 'y')

-- normal space within a line of the tune's score
scoreSpace :: Parser Char
scoreSpace =
  -- tab <|> space
  (char '\t') <|> space

space :: Parser Char--
space = char ' '

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

{- This is an area where the spec is uncertain.  See 6.1.1 Typesetting line-breaks
   The forward slash is used to indicate 'continuation of input lines' often because
   users may need to avoid long lines if, for example, they would otherwise extend
   beyond the limit of an old email system.  All very out of date, but nevertheless
   still prevalent in the wild.  We take the view that we must do our best to recognise
   them and then throw them away (along with any other later stuff in the line)

   Any text between the forward slash and eol is treated as comment.

   Return  (Continuation comment) if we have a continuation.  Now we consume the
   eol character at the end of the continuation so that the parser will continue
   to accumulate the following line into the ADT as a continuation of this line.
-}
continuation :: Parser Music
continuation =
    Continuation
        <$ char '\\'
        <*> regex "[^\x0D\n]*"
        <* eol
        <?> "continuation"

-- tune headers

headers :: Parser TuneHeaders
headers =
    many header <?> "headers"

header :: Parser Header
header =
    informationField false <* eol

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
    choice
        [
          anywhereInfo isInline
        , tuneInfo
        ]
        <?> "header"


anywhereInfo :: Boolean -> Parser Header
anywhereInfo isInline =
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

headerCode :: Char -> Parser String
headerCode c =
  let
    pattern =
       fromCharArray [ c, ':' ]
  in
    string pattern <* whiteSpace

unsupportedHeaderCode :: Parser String
unsupportedHeaderCode =
    regex "[a-qt-vx-zEJ]:" <* whiteSpace

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
    Title
        <$> ((headerCode 'T') *> (inlineInfo isInline))
        <?> "T header"


userDefined :: Boolean -> Parser Header
userDefined isInline =
    UserDefined
        <$> ((headerCode 'U') *> (inlineInfo isInline))
        <?> "U header"

voice :: Boolean -> Parser Header
voice isInline =
    buildVoice
        <$> (headerCode 'V')
        <*> alphaNumString
        <*> voiceProperties
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

{- normal Rational e.g 3/4 -}
rational :: Parser Rational
rational =
  (%) <$> int <* char '/' <*> int


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
  (choice
    [
      try suffixedTempoDesignation    -- tempo suffixed with a label
    , try unlabelledTempoDesignation  -- unlabelled tempo
    , degenerateTempo                 -- only bpm (deprecated but still prominent)
    , prefixedTempoDesignation        -- tempo prefixed with a label
    ]
  ) <* whiteSpace

{- we have for example 1/4=120 -}
unlabelledTempoDesignation :: Parser TempoSignature
unlabelledTempoDesignation =
  buildTempoSignature Nothing <$>
    tempoDesignation

{- we have for example "lento" 1/4=120 -}
prefixedTempoDesignation :: Parser TempoSignature
prefixedTempoDesignation =
  buildTempoSignature2 <$>
    spacedQuotedString <*> tempoDesignation

{- we have for example 1/4=120 "lento" -}
suffixedTempoDesignation :: Parser TempoSignature
suffixedTempoDesignation =
  flip buildTempoSignature2 <$>
    tempoDesignation <*> spacedQuotedString

{-\ we have for example 120 -}
degenerateTempo :: Parser TempoSignature
degenerateTempo =
  buildTempoSignature3 <$>
    int

tempoDesignation :: Parser TempoDesignation
tempoDesignation =
  TempoDesignation <$>
      many1 headerRational <* (char '=') <*> int

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
  buildKeySignature <$> keyName <*> option Natural sharpOrFlat <* whiteSpace <*> optionMaybe mode

{- a key accidental as an amendment to a key signature - as in e.g. K:D Phr ^f -}
keyAccidental :: Parser Pitch
keyAccidental =
  buildPitch <$> accidental <*> pitch

-- | a complete list of key accidentals which may be empty
-- | each is separated by a single space
keyAccidentals :: Parser KeySet
keyAccidentals =
    whiteSpace *> sepBy keyAccidental space

-- | (optional) properties for the Voice header
voiceProperties :: Parser (Map String String)
voiceProperties =
  Map.fromFoldable <$>
    many kvPair <* whiteSpace

-- | a key-value pair as used in a voice property
kvPair :: Parser (Tuple String String)
kvPair =
  Tuple <$>
    alphaNumString
      <*> ((char '=')
        *> (literalQuotedString <|> alphaNumString))

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

buildBar :: BarType -> List Music -> Bar
buildBar bt m =
  { startLine : bt
  , music : m
  }

buildBarTypeRecord :: Thickness -> Maybe Repeat -> Maybe Int -> BarType
buildBarTypeRecord t r i =
  { thickness : t, repeat : r, iteration : i}

{- build a bar line
   this is a bit tricky because of the poor specification for the possible shapes of bar lines
   which may have multiple different types of bar line markers (|,[,]) and repeat markers (:)
   Try to normalise to representations of basic shapes like (|, |:, :|, :||, ||:, ||, :|:, :||: )

-}
buildBarline :: String -> Maybe Int -> BarType
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
            map f (toCharArray s)

        -- count the repeat markers
        repeatCount =
            Array.length (Array.filter (\c -> c == ':') normalised)

        -- set the repeat
        repeat =
            if (repeatCount == 0) then
                Nothing
            else if (repeatCount == 1) then
                if includes ":|" (fromCharArray normalised) then
                    Just End
                else
                    Just Begin
            else
                Just BeginAndEnd
    in
      { thickness : thickness, repeat : repeat, iteration : i }

-- | a bar type for an introductory 'bar' where there is no opening bar line
invisibleBarType :: BarType
invisibleBarType =
    { thickness : Invisible
    , repeat : Nothing
    , iteration : Nothing
    }

buildBrokenOperator :: String -> Broken
buildBrokenOperator s =
    if startsWith "<" s then
        LeftArrow (length s)
    else
        RightArrow (length s)

buildRest :: Rational -> AbcRest
buildRest r =
  { duration : r }

buildGrace :: Boolean -> Nel.NonEmptyList AbcNote -> Grace
buildGrace isAcciaccatura ns =
  { isAcciaccatura, notes: ns }

buildGraceableNote :: Maybe Grace -> List String -> AbcNote -> GraceableNote
buildGraceableNote maybeGrace decs n =
  { maybeGrace, decorations : decs, abcNote : n }

buildNote :: Maybe Accidental -> String -> Int -> Maybe Rational -> Maybe Char -> AbcNote
buildNote macc pitchStr octave ml mt =
  let
    l =
      fromMaybe (1 % 1) ml
    -- a = buildAccidental macc
    pc =
      lookupPitch (toUpper pitchStr)
    spn =
      scientificPitchNotation pitchStr octave
    tied =
      case mt of
        Just _ ->
          true
        _ ->
          false
    acc =
      case macc of
        Nothing -> Implicit
        Just a -> a
  in
    { pitchClass : pc, accidental : acc, octave : spn, duration : l, tied : tied }


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

buildPitch :: Accidental -> String -> Pitch
buildPitch a pitchStr =
    Pitch { pitchClass : lookupPitch pitchStr, accidental : a }

buildChord :: Nel.NonEmptyList AbcNote -> Maybe Rational -> AbcChord
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
buildRationalFromSlashList :: forall a. Nel.NonEmptyList a -> Rational
buildRationalFromSlashList xs =
  let
    f i = (1 % (pow 2 i))
  in
    f $ Nel.length xs

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

{- default tempo signature builder which builds from
     an optional label
     a tempo designation such as (1/4=120) or (1/3 1/6=120)
-}
buildTempoSignature :: Maybe String -> TempoDesignation ->  TempoSignature
buildTempoSignature marking td  =
 case td of
   TempoDesignation noteLengths bpm ->
    { noteLengths : noteLengths
    , bpm : bpm
    , marking : marking
    }

{-| equivalent builder where we have a defined label -}
buildTempoSignature2 :: String -> TempoDesignation ->  TempoSignature
buildTempoSignature2 marking td  =
  buildTempoSignature (Just marking) td

{- builder for a degenerate tempo signature where we only have bpm -}
buildTempoSignature3 :: Int ->  TempoSignature
buildTempoSignature3 bpm  =
  let
    noteLengths =
      Nel.singleton (1 % 4)
  in
   { noteLengths : noteLengths
   , bpm : bpm
   , marking : Nothing
   }


{- build a key signature -}
buildKeySignature :: String -> Accidental -> Maybe Mode -> KeySignature
buildKeySignature pStr ma mm =
    { pitchClass : lookupPitch pStr, accidental : ma, mode : fromMaybe Major mm }

{- build a complete key designation (key signature plus modifying accidentals) -}
buildKey :: String -> KeySignature -> List Pitch -> Header
buildKey code ks pitches =
    Key { keySignature: ks, modifications: pitches }


buildVoice :: String -> String -> Map String String -> Header
buildVoice code id properties  =
    Voice { id, properties }

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

-- regex parsers.  Place some at the top level so that we can precompile the regex
brokenRhythmOperator :: Parser String
brokenRhythmOperator =
  regex "(<+|>+)"

tupletLength :: Parser String
tupletLength =
  regex "[2-9]"

endOfLine :: Parser String
endOfLine =
  regex "\r\n"

anyInt :: Parser String
anyInt =
  regex "(0|[1-9][0-9]*)"

-- low level

alphaNumString :: Parser String
alphaNumString =
  (fromCharArray <<< Array.fromFoldable <<< Nel.toList)
    <$> (whiteSpace *> many1 (alphaNum <|> char '-'))

{-| Parse a `\n` character. -}
newline :: Parser Char
newline = satisfy ((==) '\n') <?> "expected newline"


{-| Parse a `\r\n` sequence, returning a `\n` character. -}
crlf :: Parser Char
crlf = '\n' <$ endOfLine <?> "expected crlf"


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
  fromMaybe 1 <$>  -- the anyInt regex will always provide an integer if it parses
    fromString <$>
    anyInt
    <?> "expected a positive integer"

-- | quoted string parses a normal string bracketed by a '"' quote
-- | and returns just the raw string
quotedString :: Parser String
quotedString =
    string "\""
       *> regex "(\\\\\"|[^\"\n])*"
       <* string "\""
       <?> "quoted string"

-- | literal quoted String retains the quotes in the returned String
literalQuotedString :: Parser String
literalQuotedString =
  (\s -> "\"" <> s <> "\"") <$>

-- | ditto where it may be prefaced by spaces
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

-- | Run a parser for an input string, returning either a positioned error or a result.
runParser1 :: forall a. Parser a -> String -> Either PositionedParseError a
runParser1 (Parser p) s =
  let
    formatErr :: { pos :: Pos, error :: ParseError } -> PositionedParseError
    formatErr { pos : pos, error : ParseError e } =
      PositionedParseError { pos : pos, error : e}
  in
    bimap formatErr _.result (p { str: s, pos: 0 })

-- | Entry point - Parse an ABC tune image.
parse :: String -> Either PositionedParseError AbcTune
parse s =
    case runParser1 abc s of
        Right n ->
            Right n

        Left e ->
            Left e

parseKeySignature :: String -> Either PositionedParseError ModifiedKeySignature
parseKeySignature s =
    case runParser1 keySignature s of
        Right ks ->
          let
             emptyList = Nil :: List Pitch
          in
             Right { keySignature: ks, modifications: emptyList }

        Left e ->
            Left e

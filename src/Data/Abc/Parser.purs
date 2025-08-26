-- | An ABC Parser.
module Data.Abc.Parser
  ( parse
  , parseKeySignature
  ) where

import Data.Abc
import Data.Abc.Meter as Meter

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Functor (map)
import Data.Int (fromString, pow)
import Data.List (List(..), (:))
import Data.List (length) as L
import Data.List.NonEmpty as Nel
import Data.Map (Map, empty)
import Data.Map (fromFoldable) as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Rational (Rational, fromInt, (%))
import Data.String (drop, toUpper)
import Data.String.CodePoints (length)
import Data.String.CodeUnits (charAt, fromCharArray, toCharArray)
import Data.String.Utils (startsWith, includes)
import Data.Tuple (Tuple(..))
import Data.Unfoldable1 (replicate1A)
import Prelude (bind, flip, join, max, pure, ($), (*>), (+), (-), (<$), (<$>), (<*), (<*>), (<<<), (<>), (==))
import StringParser (Parser, ParseError, runParser, try)
import StringParser.CodePoints (satisfy, string, alphaNum, char, eof, regex)
import StringParser.Combinators (between, choice, many, many1, manyTill, option, optional, optionMaybe, sepBy, sepBy1, (<?>))

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

abc :: Parser AbcTune
abc =
  { headers:_, body:_ }
    <$> headers <*> body

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
    (introLine <|> fullyBarredLine)
    <?> "score"

bar :: Parser Bar
bar =
  { decorations:_, startLine:_, music:_ }
    <$> decorations
    <*> barline
    <*> (many scoreItem)
    <?> "bar"

-- | an intro bar is a bar at the beginning of a line which has no starting bar line
introBar :: Parser Bar
introBar =
  { decorations: Nil, startLine: invisibleBarType, music:_ }
    <$> many scoreItem
    <?> "intro bar"

  where
  -- | a bar type for an introductory 'bar' where there is no opening bar line
  invisibleBarType :: BarLine
  invisibleBarType =
    { endRepeats: 0
    , thickness: Invisible
    , startRepeats: 0
    , iteration: Nothing
    }

-- | an intro line as a full line of bars thus introduced
introLine :: Parser (List Bar)
introLine =
  (:) <$> introBar <*> manyTill bar eol
    <?> "intro line"

-- | a fully barred line has bar lines both at begin and end
fullyBarredLine :: Parser (List Bar)
fullyBarredLine =
  manyTill bar eol
    <?> "fully barred line"

scoreItem :: Parser Music
scoreItem =
  choice
    [ try chord -- potential ambiguity with (inline) in-score headers and slur brackets
    , try inline
    , continuation
    , try decoratedSpace -- potential ambiguity with a decorated note
    , ignore
    , spacer
    , try annotation -- potential ambiguity with chordSymbol
    , chordSymbol
    , try tuplet -- potential ambiguity with slurs inside a note
    , try brokenRhythmPair -- potential ambiguity with note and with rest
    , rest  
    , try note -- potential ambiguity with decorations on bars
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
    <$> leftSlurBrackets
    <*> decorations
    <*> (between (char '[') (char ']') (many1 (abcNote <* whiteSpace)))
    <*> optionMaybe noteDur
    <*> rightSlurBrackets
    <?> "ABC chord"

  where 
  buildChord :: Int -> List String -> Nel.NonEmptyList AbcNote -> Maybe Rational -> Int -> AbcChord
  buildChord leftSlurs decs ns ml rightSlurs =
    let
      l =
        fromMaybe (fromInt 1) ml
    in
      { leftSlurs, decorations: decs, notes: ns, duration: l, rightSlurs }

inline :: Parser Music
inline =
  Inline
    <$> between (char '[') (char ']') (tuneBodyInfo true)
    <?> "inline header"

barline :: Parser BarLine
barline =
  choice
    [ try normalBarline -- ambiguity of :: caused by degenerateDoubleColon
    , degenerateDoubleColon
    , degenerateBarVolta
    ]

{- a normal bar line (plus optional repeat iteration marker)
   see comments in 4.8 Repeat/bar symbols:
   Abc parsers should be quite liberal in recognizing bar lines. In the wild, bar lines 
   may have any shape, using a sequence of | (thin bar line), [| or |] (thick bar line), 
   and : (dots), e.g. |[| or [|:::
-}
normalBarline :: Parser BarLine
normalBarline =
  { endRepeats:_, thickness:_, startRepeats:_, iteration:_ }
    <$> repeatMarkers
    <*> barlineThickness
    <*> repeatMarkers
    <*> optionMaybe repeatSection
    <?> "bartype"

{- sometimes in the wild we get a degenerate volta marker at the start of a line 
   of music like this:
     [1 .....
   or
     _[1 ....
   again we have to be careful about ambiguity between this and inline headers by 
   making sure we parse '[' immediately followed by '1' etc.
   We treat this as a bar on its own.  If you try to treat it as a free-standing volta
   then you are beset by the ambiguity issues.
-}
degenerateBarVolta :: Parser BarLine
degenerateBarVolta =
  { endRepeats: 0, thickness: Thin, startRepeats: 0, iteration:_}
    <$> (Just <$> (whiteSpace *> char '[' *> repeatSection))

{- Parse a degenerate barline with no bar line!  Just :: on its own -}
degenerateDoubleColon :: Parser BarLine
degenerateDoubleColon =
  { endRepeats: 1, thickness: Thin, startRepeats: 1, iteration: Nothing }
    <$ char ':'
    <* char ':'

{- a repeat section at the start of a bar.  We have just parsed a bar marker (say |) and so the combination of this and the repeat may be:
      |1
      |[1
      | [1
   but is not allowed to be
      | 1
  
   repeats of the form 1,2,3 are also accepted
   associating the digit with the bracket bar number should remove ambiguity with respect to other productions that use the bracket
   (in particular, inline headers and chords).
-}
repeatSection :: Parser (Nel.NonEmptyList Volta)
repeatSection =
  sepBy1 volta (char ',')

volta :: Parser Volta
volta =
  try voltaRange
    <|> simpleVolta

voltaRange :: Parser Volta
voltaRange =
  VoltaRange <$> digit <*> (char '-' *> digit)
    <?> "volta range"

simpleVolta :: Parser Volta
simpleVolta =
  Volta <$> digit
    <?> "simple volta"

barlineThickness :: Parser Thickness
barlineThickness =
  choice
    [ ThickThin <$ string "[|"
    , ThinThick <$ string "|]"
    , ThickThin <$ string "]|"
    , ThinThin <$ string "||"
    , Thin <$ string "|"
    ]

repeatMarkers :: Parser Int
repeatMarkers =
  L.length <$> many (char ':')

-- spec is unclear if spaces are allowed after a broken rhythm operator but it's easy to support, is more permissive and doesn't break anything
brokenRhythmTie :: Parser Broken
brokenRhythmTie =
  buildBrokenOperator <$> degenerateBrokenRhythmOperator <* whiteSpace

-- | In the wild, we can see slurs encompassing the operator.  For example,
-- | instead of A>(BC) we can see  A(>BC)
-- | instead of (AB)>C we can see  (AB>)C
-- | Be lenient but throw the slur away
degenerateBrokenRhythmOperator :: Parser String
degenerateBrokenRhythmOperator =
  optional leftBracket *> brokenRhythmOperator <* optional rightBracket

brokenRhythmPair :: Parser Music
brokenRhythmPair =
  BrokenRhythmPair
    <$> restOrNote
    <*> brokenRhythmTie
    <*> restOrNote
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

  where 
  buildNote :: Maybe Accidental -> String -> Int -> Maybe Rational -> Maybe Char -> AbcNote
  buildNote macc pitchStr octave ml mt =
    let
      l =
        fromMaybe (1 % 1) ml
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
      { pitchClass: pc, accidental: acc, octave: spn, duration: l, tied: tied }

graceableNote :: Parser GraceableNote
graceableNote =
  { maybeGrace:_, leftSlurs:_, decorations:_, abcNote:_, rightSlurs:_ }
    <$> optionMaybe graceBracket
    <*> leftSlurBrackets
    <*> decorations
    <*> abcNote
    <*> rightSlurBrackets
    <?> "graceable note"

{- maybe an accidental defining a note's pitch -}
maybeAccidental :: Parser (Maybe Accidental)
maybeAccidental =
  optionMaybe accidental

accidental :: Parser Accidental
accidental =
  buildAccidental
    <$>
      ( choice
          [ string "^^"
          , string "__"
          , string "^"
          , string "_"
          , string "="
          ]
      )

  where    
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
    up = Array.length $ Array.filter ((==) '\'') (toCharArray s)
    down = Array.length $ Array.filter ((==) ',') (toCharArray s)
  in
    up - down

{- the duration of a note in the body
   order of choices here is important to remove ambiguity
-}
noteDur :: Parser Rational
noteDur =
  choice
    [ try manySlashes
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

{-}
-- | this implements the spec - a tie attaches to the end of the note and is
-- | not free standing
maybeTie :: Parser (Maybe Char)
maybeTie =
    (optionMaybe (char '-'))
        <?> "tie"
-}

-- | but here we relax the spec.  The tie still attaches to the previous note
-- | bur can now be separated from it with spaces.  It can thus appear to attach
-- | to the next note syntactically.  This helps a good deal of 'bad' ABC
-- | examples in the wild.
maybeTie :: Parser (Maybe Char)
maybeTie =
  map (\_ -> '-') <$>
    (optionMaybe (regex " *-"))
    <?> "tie"

rest :: Parser Music
rest =
  Rest
    <$> abcRest
    <?> "rest"

abcRest :: Parser AbcRest
abcRest =
  { duration:_} 
    <$> (fromMaybe (fromInt 1) <$> (regex "[XxZz]" *> optionMaybe noteDur))
    <?> "abcRest"

tuplet :: Parser Music
tuplet = do
  maybeGrace <- optionMaybe graceBracket
  leftBracketCount <- tupletBrackets
  -- calculate the number of slurs by subtracting the tuplet bracket
  let
    leftSlurs = max 0 (leftBracketCount - 1)
  signature <- tupletSignature
  -- ensure that the contents match the signature count
  -- we also need to allow whitespace betseen any of the contents but not after the final note
  restsOrNotes <- counted signature.r (whiteSpace *> restOrNote)
  pure $ Tuplet { maybeGrace, leftSlurs, signature, restsOrNotes }

-- | tuplets may now contain either a (Left) rest or a (Right) Note
restOrNote :: Parser RestOrNote
restOrNote =
  (Left <$> abcRest) <|> (Right <$> graceableNote)

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

-- | left and right slurs.  We now attach the (optional) slur brackets to thr
-- | actual target note which respectively starts or ends the slurred sequence
-- | this may be prefaced by an optional grace note - e.g.(fg)(abc)
-- | here the openinhg slur is attached to note a and the final slur to c
leftSlurBrackets :: Parser Int
leftSlurBrackets =
  L.length
    <$> many leftBracket
    <?> "left slurs"

-- | ditto for tuplets - a mandatory left bracket possibly preceded by slurs 
tupletBrackets :: Parser Int
tupletBrackets =
  Nel.length
    <$> many1 leftBracket
    <?> "tuplet + slurs"

leftBracket :: Parser Char
leftBracket =
  char '('

rightSlurBrackets :: Parser Int
rightSlurBrackets =
  L.length
    <$> many rightBracket
    <?> "right slurs"

rightBracket :: Parser Char
rightBracket =
  char ')'

graceBracket :: Parser Grace
graceBracket =
  between (char '{') (char '}') grace <* whiteSpace
    <?> "grace bracket"

grace :: Parser Grace
grace =
  { isAcciaccatura:_, notes:_ } 
    <$> acciaccatura <*> (many1 abcNote)

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

  where 
  buildAnnotation :: String -> Music
  buildAnnotation s =
    let
      placement =
        case (charAt 0 s) of
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
      Annotation placement (drop 1 s)

annotationString :: Parser String
annotationString =
  -- (\s -> "\"" <> s <> "\"") <$>
  string "\""
    *> regex "[\\^\\>\\<-@](\\\\\"|[^\"\n])*"
    <* string "\""
    <?> "annotation"

-- | a free - format chord symbol - see 4.18 Chord symbols.  Drop the quotes round the string.
chordSymbol :: Parser Music
chordSymbol =
  (ChordSymbol <<< { name:_, duration: Nothing })
    <$> literalQuotedString false
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
  between (char '!') (char '!') (regex "[^\x0D\n!]+")
    <?> "long decoration"

-- | our whiteSpace differs from that of the string parser we do NOT want to
-- |consume carriage returns or newlines
whiteSpace :: Parser String
whiteSpace =
  (fromCharArray <<< Array.fromFoldable)
    <$> many scoreSpace


{-}
whiteSpace :: Parser String
whiteSpace =
  foldMap (singleton <<< codePointFromChar) <$>
    many scoreSpace
-}

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
  DecoratedSpace <$> decorations <* (char 'y')

-- normal space within a line of the tune's score
scoreSpace :: Parser Char
scoreSpace =
  -- tab <|> space
  (char '\t') <|> space

space :: Parser Char --
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
    [ tuneBodyOnlyInfo isInline
    , anywhereInfo isInline
    ]
    <?> "tune body info"

tuneBodyOnlyInfo :: Boolean -> Parser Header
tuneBodyOnlyInfo isInline =
  choice
    [ symbolLine isInline
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
    [ anywhereInfo isInline
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
    , voice
    , wordsAfter isInline
    , fieldContinuation
    , commentLine
    ]
    <?> "anywhere info"

tuneInfo :: Parser Header
tuneInfo =
  choice
    [ area
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
    , unsupportedHeader -- headers that are currently unsupported but must be recognized and ignored
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

{- Full comment lines.  Comments are introduced with '%' and can occur anywhere
   and carry on thill the end of the line. We'll treat single line comments 
   as Headers so as not to pollute the parse tree overmuch.
   The stylesheet directive '%%' is not recognized here and will simply be
   treated as a comment.  
-}
commentLine :: Parser Header
commentLine =
  Comment
    <$> comment
    <?> "comment line"

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
  Key <$> 
    ({ keySignature:_, modifications:_, properties:_ }
      <$ (headerCode 'K')
      <*> keySignature
      <*> keyAccidentals
      <*> amorphousProperties
      <?> "K header"
    )

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

voice :: Parser Header
voice =
  Voice <$> 
    ( { id:_, properties:_ }
        <$ (headerCode 'V')
        <*> alphaNumPlusString
        <*> amorphousProperties
        <?> "V header"    
    )

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
    <$> ((headerCode 'X') *> (optionMaybe int))
    <* whiteSpace
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

meterDefinition :: Parser (Maybe TimeSignature)
meterDefinition =
  choice
    [ cutTime
    , commonTime
    , timeSignature
    , nometer
    ]

commonTime :: Parser (Maybe TimeSignature)
commonTime =
  (Just Meter.commonTime) <$ char 'C'

cutTime :: Parser (Maybe TimeSignature)
cutTime =
  (Just Meter.cutTime) <$ string "C|"

timeSignature :: Parser (Maybe TimeSignature)
timeSignature =
  Just <$> ( { numerator:_, denominator:_} 
    <$> int <* char '/' <*> int <* whiteSpace)

nometer :: Parser (Maybe TimeSignature)
nometer =
  Nothing <$ string "none"

tempoSignature :: Parser TempoSignature
tempoSignature =
  ( choice
      [ try suffixedTempoDesignation -- tempo suffixed with a label
      , try unlabelledTempoDesignation -- unlabelled tempo
      , degenerateTempo -- only bpm (deprecated but still prominent)
      , prefixedTempoDesignation -- tempo prefixed with a label
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
  buildTempoSignature2
    <$> spacedQuotedString
    <*> tempoDesignation

{- we have for example 1/4=120 "lento" -}
suffixedTempoDesignation :: Parser TempoSignature
suffixedTempoDesignation =
  flip buildTempoSignature2
    <$> tempoDesignation
    <*> spacedQuotedString

{-\ we have for example 120 -}
degenerateTempo :: Parser TempoSignature
degenerateTempo =
  buildTempoSignature3 <$>
    int

tempoDesignation :: Parser TempoDesignation
tempoDesignation =
  TempoDesignation
    <$> many1 headerRational
    <* (char '=')
    <*> int

sharpOrFlat :: Parser Accidental
sharpOrFlat =
  map
    ( \x ->
        if x == '#' then
          Sharp
        else
          Flat
    )
    (char '#' <|> char 'b')

keyName :: Parser String
keyName =
  regex "[A-G]"

keySignature :: Parser KeySignature
keySignature =
  buildKeySignature <$> keyName <*> option Natural sharpOrFlat <* whiteSpace <*> optionMaybe mode

  where 
  buildKeySignature :: String -> Accidental -> Maybe Mode -> KeySignature
  buildKeySignature pStr ma mm =
    { pitchClass: lookupPitch pStr, accidental: ma, mode: fromMaybe Major mm }

{- a key accidental as an amendment to a key signature - as in e.g. K:D Phr ^f -}
keyAccidental :: Parser Pitch
keyAccidental =
  buildPitch <$> accidental <*> pitch

  where 
  buildPitch :: Accidental -> String -> Pitch
  buildPitch a pitchStr =
    Pitch { pitchClass: lookupPitch pitchStr, accidental: a }

-- | a complete list of key accidentals which may be empty
-- | each is separated by a single space
keyAccidentals :: Parser KeySet
keyAccidentals =
  whiteSpace *> sepBy keyAccidental space

-- | (optional) properties for the Voice or Key header
amorphousProperties :: Parser (Map String String)
amorphousProperties =
  Map.fromFoldable
    <$> many kvPair
    <* whiteSpace

-- | a key-value pair as used in a voice property
kvPair :: Parser (Tuple String String)
kvPair =
  Tuple
    <$> alphaNumPlusString
    <*>
      ( (char '=')
          *> (spacedQuotedString <|> alphaNumPlusString)
      )

mode :: Parser Mode
mode =
  choice
    [ try major
    , ionian
    , dorian
    , phrygian
    , lydian
    , mixolydian
    , aeolian
    , locrian
    , minor -- place 'last' because of potential ambiguity
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

buildBrokenOperator :: String -> Broken
buildBrokenOperator s =
  if startsWith "<" s then
    LeftArrow (length s)
  else
    RightArrow (length s)

-- shared builders

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
        2 -> 3
        3 -> 2
        4 -> 3
        6 -> 2
        8 -> 3
        _ -> 2

    q =
      fromMaybe qdefault (map toTupletInt mq)

    r =
      fromMaybe p (map toTupletInt mr)
  in
    { p, q, r }

toTupletInt :: String -> Int
toTupletInt s =
  fromMaybe 3 (fromString s)

{- default tempo signature builder which builds from
     an optional label
     a tempo designation such as (1/4=120) or (1/3 1/6=120)
-}
buildTempoSignature :: Maybe String -> TempoDesignation -> TempoSignature
buildTempoSignature marking td =
  case td of
    TempoDesignation noteLengths bpm ->
      { noteLengths: noteLengths
      , bpm: bpm
      , marking: marking
      }

{-| equivalent builder where we have a defined label -}
buildTempoSignature2 :: String -> TempoDesignation -> TempoSignature
buildTempoSignature2 marking td =
  buildTempoSignature (Just marking) td

{- builder for a degenerate tempo signature where we only have bpm -}
buildTempoSignature3 :: Int -> TempoSignature
buildTempoSignature3 bpm =
  let
    noteLengths =
      Nel.singleton (1 % 4)
  in
    { noteLengths: noteLengths
    , bpm: bpm
    , marking: Nothing
    }

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

-- regex parsers.  
brokenRhythmOperator :: Parser String
brokenRhythmOperator =
  regex "(<+|>+)"

tupletLength :: Parser String
tupletLength =
  regex "[2-9]"

anyInt :: Parser String
anyInt =
  regex "(0|[1-9][0-9]*)"

anyDigit :: Parser String
anyDigit =
  regex "([0-9])"

-- low level

{- an alphnumeric string with added +,- and _ -}
alphaNumPlusString :: Parser String
alphaNumPlusString =
  (fromCharArray <<< Array.fromFoldable <<< Nel.toList)
    <$> (many1 (alphaNum <|> char '-' <|> char '+' <|> char '_') <* whiteSpace)

{-| Parse a `\n` character which may terminate a line in some systems. -}
newline :: Parser Char
newline = satisfy ((==) '\n') <?> "expected newline"

-- | Parse a conventional carriage return, linefeedsequence, returning a `\n` character. 
-- | However, also accommodate the non-standard and deprecated exclamation mark which 
-- | is used to indicate line-breaks in older systems. See 10.2.1 Outdated line-breaking.
-- | Note that this brings ambiguity with respect to long decorations which are also introuduced
-- | by exclamation marks, but incorporating it into a regex means we don't need to protect
-- | it with a try, because the whole regex match is either consumed or not.
-- | Also accommodate a carriage return but without the terminating newline
crlf :: Parser Char
crlf = '\n' <$ regex "!?\r(\n)?" <?> "expected crlf"

{-| Parse an end of line character or sequence, returning a `\n` character. 
    Before the actual end of line, we can have comments, which are discarded
-}
eol :: Parser Char
eol =
  optional comment *>
    crlf <|> newline

{- Parse a comment.  These can only occur at the end of a line-}
comment :: Parser String
comment =
  char '%' *> commentStrToEol

{- parse a remaining string up to but not including the end of line
   here we intend to retain the string so we bar any comments
-}
strToEol :: Parser String
strToEol =
  regex "[^\x0D\n%]*"

{- as above but with further comment characters allowed -}
commentStrToEol :: Parser String
commentStrToEol =
  regex "[^\x0D\n]*"

{-| Parse a positive integer (with no sign). -}
int :: Parser Int
int =
  fromMaybe 1
    <$> -- the anyInt regex will always provide an integer if it parses
      fromString
    <$>
      anyInt
    <?> "expected a positive integer"

{-| Parse a digit (with no sign). -}
digit :: Parser Int
digit =
  fromMaybe 1
    <$> fromString
    <$>
      anyDigit
    <?> "expected a digit"

-- | literal quoted String. Optionally retain the quotes surrounding the returned String
literalQuotedString :: Boolean -> Parser String
literalQuotedString retainQuotes =
  let
    quotedString :: Parser String
    quotedString =
      string "\""
        *> regex "(\\\\\"|[^\"\n])*"
        <* string "\""
        <?> "quoted string"
  in
    if retainQuotes then
      (\s -> "\"" <> s <> "\"") <$> quotedString
    else
      quotedString

-- | ditto where it may be bracketed by spaces
spacedQuotedString :: Parser String
spacedQuotedString =
  try -- whitespace can crop up anywhere
    (whiteSpace *> (literalQuotedString true) <* whiteSpace)

-- utility Functions

-- | Specialize replicate1A to the Parser i order to allow a counted
-- | number of instances of the parser
counted :: ∀ a. Int -> Parser a -> Parser (Nel.NonEmptyList a)
counted num parser =
  replicate1A num parser

-- | Parse an ABC tune image.
parse :: String -> Either ParseError AbcTune
parse s =
  runParser abc s 

-- | Parse an ABC key signature
parseKeySignature :: String -> Either ParseError ModifiedKeySignature
parseKeySignature s =
  case runParser keySignature s of
    Right ks ->
      let
        emptyList = Nil :: List Pitch
      in
        Right { keySignature: ks, modifications: emptyList, properties: empty }

    Left e ->
      Left e

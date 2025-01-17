-- | Transposition of an ABC note or tune to a new key.
module Data.Abc.Transposition
  ( defaultKey
  , keyDistance
  , transposeNote
  , transposeTo
  ) where

{- A parse tree score contains pitches with accidentals of type Implicit in cases where no accidental is explitly marked
   (sharp, flat, natural etc.).  Such pitches inherit their accidental nature firstly from any preceding note of the same pitch
   in the same bar which has an explicit marking and secondly from the key signature.  During transposition we must firstly
   make such accidentals explicit, transpose them and finally return to their implicit nature.
   This means we have to thread state through the transposition
-}

import Data.Abc

import Control.Monad.State (State, evalStateT, get, put)
import Data.Abc.Accidentals as Accidentals
import Data.Abc.KeySignature (diatonicScale, getKeyProps, getKeySig, isCOrSharpKey, modifiedKeySet, notesInChromaticScale, pitchNumbers, pitchNumber, inKeySet, transposeKeySignatureBy)
import Data.Either (Either(..))
import Data.Foldable (oneOf)
import Data.List (List(..), (:), filter, reverse)
import Data.List.NonEmpty (NonEmptyList) as Nel
import Data.Map (Map, empty, fromFoldable, lookup)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst)
import Prelude (($), (+), (-), (==), (/=), (&&), (||), (<), (<=), (>=), bind, map, mod, negate, pure)

type TranspositionState =
  { keyDistance :: Int -- semitone distance between keys - may be positive or negative
  , sourcemks :: ModifiedKeySignature -- source key signature
  , sourceKeyProps :: AmorphousProperties -- source key properties
  , sourceBarAccidentals :: Accidentals.Accidentals -- any accidental defined locally to the current bar in the tune source
  , targetmks :: ModifiedKeySignature -- target key signature
  , targetKeySet :: KeySet -- the set of accidental keys in the target key signature
  , targetScale :: KeySet -- diatonic scale in the target key
  , targetBarAccidentals :: Accidentals.Accidentals -- any accidental defined locally to the current bar in the tune target
  }

type Transposition a = State TranspositionState a

type NoteIndex =
  { notePosition :: Int -- note position in the appropriate diatonic scale
  , octaveIncrement :: Int -- octave increment -1 <= i <= 1
  }

-- Exposed API

-- | The default Key - C Major.
defaultKey :: ModifiedKeySignature
defaultKey =
  { keySignature: { pitchClass: C, accidental: Natural, mode: Major }
  , modifications: Nil
  , properties: empty
  }

-- | Calculate the distance between the keys (target - source) measured in semitones.
-- | Keys must be in compatible modes.
-- | not sure whether we need to expose this
keyDistance :: ModifiedKeySignature -> ModifiedKeySignature -> Either String Int
keyDistance targetmks srcmks =
  let
    target =
      targetmks.keySignature

    src =
      srcmks.keySignature

    targetAcc =
      target.accidental

    srcAcc =
      src.accidental
  in
    if (target.mode /= src.mode) then
      Left "incompatible modes"
    else
      Right
        ( transpositionDistance
            (Pitch { pitchClass: target.pitchClass, accidental: targetAcc })
            (Pitch { pitchClass: src.pitchClass, accidental: srcAcc })
        )

runNote :: TranspositionState -> AbcNote -> AbcNote
runNote state note =
  unwrap $ evalStateT (transposeNoteBy note) state

runTune :: TranspositionState -> AbcTune -> AbcTune
runTune state tune =
  unwrap $ evalStateT (transposeTune tune) state

{-
-- | Transpose a note from its source key to its target.
transposeGraceableNote :: ModifiedKeySignature -> ModifiedKeySignature -> GraceableNote -> Either String GraceableNote
transposeGraceableNote targetmks srcKey gn =
  let
    abcNote = transposeNote targetmks srcKey gn.abcNote
    maybeGrace = map (transposeGrace keyDistance) gn.maybeGrace
  in
    { maybeGrace, abcNote}
-}

-- | Transpose a note from its source key to its target.
transposeNote :: ModifiedKeySignature -> ModifiedKeySignature -> AbcNote -> Either String AbcNote
transposeNote targetmks srcKey note =
  let
    rdist =
      keyDistance targetmks srcKey
  in
    case rdist of
      Left e ->
        Left e

      Right d ->
        let
          transpositionState =
            { keyDistance: d
            , sourcemks: srcKey
            , sourceKeyProps: empty :: AmorphousProperties
            , sourceBarAccidentals: Accidentals.empty
            , targetmks: targetmks
            , targetKeySet: modifiedKeySet targetmks
            , targetScale: diatonicScale (targetmks.keySignature)
            , targetBarAccidentals: Accidentals.empty
            }
        in
          Right $ runNote transpositionState note

-- | transposition where the target mode is taken from the source tune's key signature
-- | (it doesn't make any sense to transpose to a different mode)
transposeTo :: Pitch -> AbcTune -> AbcTune
transposeTo (Pitch targetP) t =
  let
    -- get the source key signature stuff
    mks = fromMaybe defaultKey $ getKeySig t
    keyProps = getKeyProps t
    srcAcc = mks.keySignature.accidental
    srcPc = mks.keySignature.pitchClass
    -- get the target key signature stuff, retaining the mode
    targetAcc = targetP.accidental
    targetPc = targetP.pitchClass
    targetmks =
      { keySignature: { pitchClass: targetPc, accidental: targetAcc, mode: mks.keySignature.mode }
      , modifications: Nil
      , properties: mks.properties
      }
    -- work out the distance between them
    d = transpositionDistance
      (Pitch targetP)
      (Pitch { pitchClass: srcPc, accidental: srcAcc })
  in
    -- don't bother transposing if there's no distance between the keys
    if (d == 0) then
      t
    else
      let
        transpositionState =
          { keyDistance: d
          , sourcemks: mks
          , sourceKeyProps: keyProps
          , sourceBarAccidentals: Accidentals.empty
          , targetmks: targetmks
          , targetKeySet: modifiedKeySet targetmks
          , targetScale: diatonicScale (targetmks.keySignature)
          , targetBarAccidentals: Accidentals.empty
          }
      in
        runTune transpositionState t

-- Implementation

transposeTune :: AbcTune -> Transposition AbcTune
transposeTune t =
  do
    state <- get
    let
      newHeaders :: TuneHeaders
      newHeaders =
        replaceKeyHeader state.targetmks t.headers
    newTuneBody <- transposeTuneBody t.body
    pure { headers: newHeaders, body: newTuneBody }

-- | transpose the tune body.
transposeTuneBody :: TuneBody -> Transposition TuneBody
transposeTuneBody body =
  traverse transposeBodyPart body

processHeader :: Header -> Transposition Header
processHeader h =
  case h of
    Key mks ->
      do
        state <- get
        let
          newmks =
            transposeKeySignatureBy state.keyDistance mks

          newState =
            state
              { sourcemks = mks
              , sourceKeyProps = mks.properties
              , sourceBarAccidentals = Accidentals.empty
              , targetmks = newmks
              , targetKeySet = modifiedKeySet newmks
              , targetScale = diatonicScale (newmks.keySignature)
              , targetBarAccidentals = Accidentals.empty
              }

        _ <- put newState
        pure $ (Key newmks)
    _ ->
      pure h

transposeBodyPart :: BodyPart -> Transposition BodyPart
transposeBodyPart bp =
  case bp of
    -- just transpose the score
    Score bars ->
      do
        newBars <- transposeBarList bars
        pure $ Score newBars

    -- transpose any Key header found inline
    BodyInfo h ->
      do
        newHeader <- processHeader h
        pure $ BodyInfo newHeader

transposeBarList :: List Bar -> Transposition (List Bar)
transposeBarList bars =
  traverse transposeBar bars

transposeBar :: Bar -> Transposition Bar
transposeBar bar =
  do
    state <- get
    let
      -- initialise accidentals list
      state' = state
        { sourceBarAccidentals = Accidentals.empty
        , targetBarAccidentals = Accidentals.empty
        }
    _ <- put state'
    newMusic <- transposeMusicList bar.music
    let
      newBar = bar { music = newMusic }
    pure newBar

transposeMusicList :: List Music -> Transposition (List Music)
transposeMusicList musics =
  traverse transposeMusic musics

transposeMusic :: Music -> Transposition Music
transposeMusic m =
  case m of
    Note n ->
      do
        newN <- transposeGraceableNoteBy n
        pure $ Note newN

    BrokenRhythmPair n1 b n2 ->
      do
        result1 <- transposeRestOrNoteBy n1
        result2 <- transposeRestOrNoteBy n2
        pure $ BrokenRhythmPair result1 b result2

    Tuplet tuplet ->
      do
        maybeGrace <- transposeGrace tuplet.maybeGrace
        restsOrNotes <- transposeRestOrNoteList tuplet.restsOrNotes
        let
          leftSlurs = tuplet.leftSlurs
          signature = tuplet.signature
        pure $ Tuplet { maybeGrace, leftSlurs, signature, restsOrNotes }

    Chord c ->
      do
        newC <- transposeChord c
        pure $ Chord newC

    -- we won't attempt to transpose chord symbols - just quietly drop them
    ChordSymbol _ ->
      pure Ignore

    -- an inline header
    Inline h ->
      do
        newH <- processHeader h
        pure $ Inline newH

    _ ->
      pure m

transposeNoteList :: Nel.NonEmptyList AbcNote -> Transposition (Nel.NonEmptyList AbcNote)
transposeNoteList nonemptyns =
  traverse transposeNoteBy nonemptyns

transposeRestOrNoteList :: Nel.NonEmptyList RestOrNote -> Transposition (Nel.NonEmptyList RestOrNote)
transposeRestOrNoteList nonemptyns =
  traverse transposeRestOrNoteBy nonemptyns

transposeChord :: AbcChord -> Transposition AbcChord
transposeChord c =
  do
    newNotes <- transposeNoteList c.notes
    pure $ c { notes = newNotes }

transposeRestOrNoteBy :: RestOrNote -> Transposition RestOrNote
transposeRestOrNoteBy restOrNote =
  case (restOrNote) of
    Left r ->
      pure $ Left r
    Right n -> do
      newN <- transposeGraceableNoteBy n
      pure $ Right newN

transposeGraceableNoteBy :: GraceableNote -> Transposition GraceableNote
transposeGraceableNoteBy gn =
  do
    abcNote <- transposeNoteBy gn.abcNote
    maybeGrace <- transposeGrace gn.maybeGrace
    let
      decorations = gn.decorations
      leftSlurs = gn.leftSlurs
      rightSlurs = gn.rightSlurs
    pure { maybeGrace, leftSlurs, decorations, abcNote, rightSlurs }

transposeGrace :: Maybe Grace -> Transposition (Maybe Grace)
transposeGrace mGrace =
  case
    mGrace
    of
    Just grace ->
      do
        newNotes <- transposeNoteList grace.notes
        pure $ Just grace { notes = newNotes }
    _ ->
      pure Nothing

{-| transpose a note by the required distance which may be positive or negative
    transposition distance and source and target keys are taken from the state.  This is the heart of the module.
    The strategy is:
      * does the source note have an explicit accidental?
      * if not, does it have an implicit accidental?  i.e. implied firstly by an earlier explicit in the bar or secondly by the key signature
      * move the note to the target by the required distance between the keys, using either form of accidental if present
      * use a convention for the target note that if it has an explicit accidental, the accidental in the full note is represented as Just Acc - otherwise None
      * to decide on whether an explicit accidental is present, first look up the exact note in the target bar accidentals, then do so for just the pitch
        (i.e. there is an accidental for the pitch but it's a different one) and finally look up the note in the target scale for the key in question
      * if there is an explicit accidental, save the source note in the source bar accidentals, ditto for the target, keep in the state
-}
transposeNoteBy :: AbcNote -> Transposition AbcNote
transposeNoteBy note =
  do
    state <- get
    let
      -- make any implicit accidental explicit in the source note to be transposed if it's not marked as an accidental
      inSourceKeyAccidental =
        Accidentals.implicitInKeySet note.pitchClass (modifiedKeySet state.sourcemks)

      inSourceBarAccidental =
        lookup note.pitchClass state.sourceBarAccidentals

      -- we must do the lookup of the source accidental in this order - local bar overrides key
      maybeSourceAccidental =
        oneOf (inSourceBarAccidental : inSourceKeyAccidental : Nil)

      implicitSourceAccidental = fromMaybe Implicit maybeSourceAccidental

      explicitSourceNote =
        case note.accidental of
          Implicit ->
            note { accidental = implicitSourceAccidental }
          _ ->
            note

      srcNum =
        noteNumber explicitSourceNote

      -- ( targetNum, octaveIncrement ) =  noteIndex srcNum (state.keyDistance)
      noteIdx = noteIndex srcNum (state.keyDistance)

      ka =
        pitchFromInt (state.targetmks.keySignature) noteIdx.notePosition

      -- ( pc, acc ) = sharpenFlatEnharmonic ka
      Pitch safeKa = sharpenFlatEnharmonic ka

      --JMW
      targetBarAcc = Accidentals.lookup safeKa.pitchClass state.targetBarAccidentals

      targetAcc =
        -- is it present in the local target bar accidentals
        if (Accidentals.member (Pitch safeKa) state.targetBarAccidentals) then
          Implicit
        -- is it present in the local target bar accidentals but with a different value
        -- else if (isJust (Accidentals.lookup safeKa.pitchClass state.targetBarAccidentals)) then
        else if (isJust targetBarAcc) then
          safeKa.accidental
        -- is it in the set of keys in the target diatonic scale
        else if (inKeySet (Pitch safeKa) state.targetScale) then
          Implicit
        else
          safeKa.accidental

      transposedNote =
        note
          { pitchClass = safeKa.pitchClass
          , accidental = targetAcc
          , octave = note.octave + noteIdx.octaveIncrement
          }

      -- save any accidental nature of the original untransposed note
      newSourceAccs =
        addBarAccidental note.pitchClass note.accidental state.sourceBarAccidentals

      -- if the target, after all this, has an explicit accidental, we need to save it in the target bar accidental state
      newTargetAccs =
        case targetAcc of
          Implicit ->
            state.targetBarAccidentals
          _ ->
            -- we use the explicit form of the target
            addBarAccidental safeKa.pitchClass (safeKa.accidental) state.targetBarAccidentals

      -- update the state with both the source an target bar accidentals
      newState =
        state
          { sourceBarAccidentals = newSourceAccs
          , targetBarAccidentals = newTargetAccs
          }
    _ <- put newState
    pure transposedNote

-- | enharmonic equivalence for flattened accidentals
-- | We'll (for the moment) use the convention that most flat accidentals
-- | are presented in sharpened form
sharpenFlatEnharmonic :: Pitch -> Pitch
sharpenFlatEnharmonic ka =
  case ka of
    Pitch { pitchClass: G, accidental: Flat } ->
      Pitch { pitchClass: F, accidental: Sharp }

    Pitch { pitchClass: D, accidental: Flat } ->
      Pitch { pitchClass: C, accidental: Sharp }

    _ ->
      ka

-- | we need to take note of any accidentals so far in the bar because these may influence
-- | later notes in that bar.  If the note uses an explicit accidental, add it to the accidental set.
addBarAccidental :: PitchClass -> Accidental -> Accidentals.Accidentals -> Accidentals.Accidentals
addBarAccidental pc acc accs =
  case acc of
    Implicit ->
      accs
    x ->
      Accidentals.add pc x accs

-- | note pairs for the black and white notes of a piano,
-- | designating black notes with the Sharp accidental
sharpNoteNumbers :: List (Tuple Pitch Int)
sharpNoteNumbers =
  let
    f nn =
      let
        Pitch p = fst nn
      -- pos = snd nn
      in
        ((p.accidental == Sharp) && (p.pitchClass /= E && p.pitchClass /= B))
          || (p.accidental == Natural)
  in
    filter f pitchNumbers

-- | note pairs for the black and white notes of a piano,
-- | designating black notes with the Flat accidental
flatNoteNumbers :: List (Tuple Pitch Int)
flatNoteNumbers =
  let
    f nn =
      let
        Pitch p = fst nn
      -- pos = snd nn
      in
        ((p.accidental == Flat) && (p.pitchClass /= F && p.pitchClass /= C))
          || (p.accidental == Natural)
  in
    filter f pitchNumbers

-- | given a key signature and an integer (0 <= n < notesInChromaticScale)
-- | return the pitch of the note within that signature
pitchFromInt :: KeySignature -> Int -> Pitch
pitchFromInt ks i =
  let
    dict =
      if (isCOrSharpKey ks) then
        sharpNotedNumbers
      else
        flatNotedNumbers
  in
    fromMaybe (Pitch { pitchClass: C, accidental: Natural }) $ lookup i dict

-- | the inverted lookup for sharp chromatic scales.  This dictionaary
-- | allows you to enter a number (0 <= n < notesInChromaticScale) and return
-- | a (pitchClass, Accidental) pair which is the note's pitch
sharpNotedNumbers :: Map Int Pitch
sharpNotedNumbers =
  let
    invert (Tuple a b) =
      (Tuple b a)
  in
    fromFoldable $ map invert sharpNoteNumbers

-- | the inverted lookup for flat chromatic scales.  This dictionaary
-- | allows you to enter a number (0 <= n < notesInChromaticScale) and return
-- | a (pitchClass, Accidental) pair which is the note's pitch
flatNotedNumbers :: Map Int Pitch
flatNotedNumbers =
  let
    invert (Tuple a b) =
      (Tuple b a)
  in
    fromFoldable $ map invert flatNoteNumbers

-- | look up the note and return the number of its pitch in the range 0 <= n < notesInChromaticScale (0 is C Natural) -}
noteNumber :: AbcNote -> Int
noteNumber n =
  pitchNumber (Pitch { pitchClass: n.pitchClass, accidental: n.accidental })

-- | inspect the current note index and the amount it is to be incremented by.
-- | produce a new note index in the range (0 <= n < notesInChromaticScale)
-- | and associate with this a number (-1,0,1) which indicates an increment to the octave
noteIndex :: Int -> Int -> NoteIndex
noteIndex from increment =
  let
    to =
      (from + increment)
  in
    if to < 0 then
      { notePosition: (notesInChromaticScale + to), octaveIncrement: -1 }
    else if (to >= notesInChromaticScale) then
      { notePosition: (to - notesInChromaticScale), octaveIncrement: 1 }
    else
      { notePosition: to, octaveIncrement: 0 }

-- | work out the minimum transposition distance (target - source) or (source - target) measured in semitones
transpositionDistance :: Pitch -> Pitch -> Int
transpositionDistance target source =
  let
    distance =
      (pitchNumber target - pitchNumber source)
  in
    -- +5 is a shorter distance than -7 etc
    if (distance <= -7) then
      (notesInChromaticScale + distance) `mod` notesInChromaticScale
    else
      distance

-- | replace a Key header (if it exists)
-- | and place last in the list of headers, retaining the order of the other headers
replaceKeyHeader :: ModifiedKeySignature -> TuneHeaders -> TuneHeaders
replaceKeyHeader newmks hs =
  let
    f h =
      case h of
        Key _ ->
          false

        _ ->
          true
    newhs =
      filter f hs
  in
    reverse $ (Key newmks) : (reverse newhs)

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

   This means we have to thread state through the transposition and hence use folds rather than maps
-}

import Prelude (($), (+), (-), (==), (/=), (&&), (||), (<), (<=), (>=), (<<<), map, mod, negate)
import Partial.Unsafe (unsafePartial)
import Data.Either (Either(..))
import Data.List (List(..), (:), filter, foldl, reverse)
import Data.List.NonEmpty (NonEmptyList, fromList, toList) as Nel
import Data.Map (Map, fromFoldable, lookup)
import Data.Maybe (fromMaybe, isJust, fromJust)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Foldable (oneOf)
import Data.Bifunctor (lmap)
import Data.Abc
import Data.Abc.Accidentals as Accidentals
import Data.Abc.Metadata (getKeySig)
import Data.Abc.KeySignature (diatonicScale, isCOrSharpKey, modifiedKeySet
    ,notesInChromaticScale, pitchNumbers, pitchNumber, inKeySet, transposeKeySignatureBy)

type TranspositionState =
    { keyDistance :: Int  -- semitone distance between keys - may be positive or negative
    , sourcemks :: ModifiedKeySignature -- source key signature
    , sourceBarAccidentals :: Accidentals.Accidentals -- any accidental defined locally to the current bar in the tune source
    , targetmks :: ModifiedKeySignature   -- target key signature
    , targetKeySet :: KeySet -- the set of accidental keys in the target key signature
    , targetScale :: KeySet -- diatonic scale in the target key
    , targetBarAccidentals :: Accidentals.Accidentals -- any accidental defined locally to the current bar in the tune target
    }

type NoteIndex =
  { notePosition :: Int    -- note position in the appropriate diatonic scale
  , octaveIncrement :: Int -- octave increment -1 <= i <= 1
  }

-- Exposed API


-- | The default Key - C Major.
defaultKey :: ModifiedKeySignature
defaultKey =
  { keySignature: { pitchClass: C, accidental: Natural, mode: Major }, modifications: Nil }

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
      Right (transpositionDistance
              ( Pitch { pitchClass: target.pitchClass, accidental: targetAcc })
              ( Pitch { pitchClass: src.pitchClass, accidental: srcAcc })
            )

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
            , sourceBarAccidentals: Accidentals.empty
            , targetmks: targetmks
            , targetKeySet: modifiedKeySet targetmks
            , targetScale: diatonicScale (targetmks.keySignature)
            , targetBarAccidentals: Accidentals.empty
            }

          transposition =
            (transposeNoteBy transpositionState note)
        in
          case transposition of
            Tuple transposedNote _ ->
              Right transposedNote

-- | transposition where the target mode is taken from the source tune's key signature
-- | (it doesn't make any sense to transpose to a different mode)
transposeTo :: Pitch -> AbcTune -> AbcTune
transposeTo (Pitch targetP) t =
  let
    -- get the source key signature stuff
    mks = fromMaybe defaultKey $ getKeySig t
    srcAcc = mks.keySignature.accidental
    srcPc = mks.keySignature.pitchClass
    -- get the target key signature stuff, retaining the mode
    targetAcc = targetP.accidental
    targetPc = targetP.pitchClass
    targetmks =
        { keySignature: { pitchClass: targetPc, accidental: targetAcc, mode: mks.keySignature.mode }, modifications: Nil }
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
          , sourceBarAccidentals: Accidentals.empty
          , targetmks: targetmks
          , targetKeySet: modifiedKeySet targetmks
          , targetScale: diatonicScale (targetmks.keySignature)
          , targetBarAccidentals: Accidentals.empty
          }
      in
        transposeTune transpositionState t


-- Implementation

transposeTune :: TranspositionState -> AbcTune -> AbcTune
transposeTune state t =
  let
    newHeaders =
      replaceKeyHeader state.targetmks t.headers
  in
    { headers: newHeaders, body: (transposeTuneBody state t.body) }

-- | transpose the tune body.  We need to thread state through the tune in case there's an inline
-- | information header which changes key part way through the tune
transposeTuneBody :: TranspositionState -> TuneBody -> TuneBody
transposeTuneBody state body =
  let
    f :: Tuple (List BodyPart) TranspositionState -> BodyPart -> Tuple (List BodyPart) TranspositionState
    f acc n =
      let
        bs = fst acc
        s0 = snd acc
        newAcc =
          transposeBodyPart s0 n
        b1 = fst newAcc
        s1 = snd newAcc
      in
        Tuple (b1 : bs) s1
  in
    let
      -- ( tb, news ) = List.foldl f ( [], state ) body
      result = foldl f (Tuple Nil state ) body
    in
      reverse (fst result)


processHeader :: TranspositionState -> Header -> Tuple Header TranspositionState
processHeader state h =
  case h of
    Key mks ->
      let
        newmks =
          transposeKeySignatureBy state.keyDistance mks

        newState =
          state
            { sourcemks = mks
            , sourceBarAccidentals = Accidentals.empty
            , targetmks = newmks
            , targetKeySet = modifiedKeySet newmks
            , targetScale = diatonicScale (newmks.keySignature)
            , targetBarAccidentals = Accidentals.empty
            }
      in
        Tuple
          (Key newmks)
          (newState)

    _ ->
      Tuple h state


transposeBodyPart :: TranspositionState -> BodyPart -> Tuple BodyPart TranspositionState
transposeBodyPart state bp =
  case bp of
    -- just transpose the score
    Score bars ->
      let
        -- ( ms1, s1 ) = transposeMusicList state ms
        result = transposeBarList state bars
        bs1 = fst result
        s1 = snd result
      in
        Tuple (Score bs1) s1

    -- transpose any Key header found inline
    BodyInfo h ->
      let
        -- ( h1, state1 ) = processHeader state h
        result = processHeader state h
        h1 = fst result
        state1 = snd result
      in
        Tuple (BodyInfo h1) state1


transposeMusic :: TranspositionState -> Music -> Tuple Music TranspositionState
transposeMusic state m =
  case m of
    Note n ->
      let
        result = transposeNoteBy state n
      in
        Tuple (Note $ fst result) (snd result)

    BrokenRhythmPair n1 b n2 ->
      let
        result1 = transposeNoteBy state n1
        s1 = snd result1
        result2 = transposeNoteBy s1 n2
      in
        Tuple (BrokenRhythmPair (fst result1) b (fst result2)) (snd result2)

    Tuplet ts ns ->
      let
        result = transposeRestOrNoteList state ns
      in
        Tuple ( Tuplet ts (fst result)) (snd result)

    GraceNote b ns ->
      let
        result = transposeNoteList state ns
      in
        Tuple ( GraceNote b (fst result)) (snd result)

    Chord c ->
      let
        result = transposeChord state c
      in
        Tuple (Chord (fst result)) (snd result)

    -- we won't attempt to transpose chord symbols - just quietly drop them
    ChordSymbol s ->
      Tuple Ignore state

    -- new bar, initialise accidentals list
    {-}
    Barline b ->
      Tuple ( Barline b)
          ( state  { sourceBarAccidentals = Accidentals.empty
                   , targetBarAccidentals = Accidentals.empty } )
    -}

    -- an inline header
    Inline h ->
      let
        result = processHeader state h
      in
        Tuple (Inline (fst result)) (snd result)

    _ ->
      Tuple m state

{- generic attempt
transposeList :: forall m. TranspositionState -> (TranspositionState -> m -> m) -> List m -> Tuple (List m) TranspositionState
transposeList state transposef ns =
    let
        f :: forall m. Tuple (List m) TranspositionState -> m -> Tuple (List m) TranspositionState
        f acc n =
            let
                -- ( ns, s0 ) = acc
                ns = fst acc
                s0 = snd acc

                -- ( n1, s1 ) = transposeNoteBy s0 n
                result :: Tuple m TranspositionState
                result = transposef s0 n
                n1 = fst result
                s1 = snd result
            in
                -- ( n1 :: ns, s1 )
                Tuple (n1 : ns) s1
    in
        let
            -- ( tns, news ) = List.foldl f ( [], state ) ns
            res = foldl f (Tuple Nil state) ns
        in
            -- ( List.reverse tns, news )
            Tuple (reverse $ fst res) (snd res)
-}


transposeBarList :: TranspositionState -> List Bar -> Tuple (List Bar) TranspositionState
transposeBarList state bs =
  let
    f :: Tuple (List Bar) TranspositionState -> Bar -> Tuple (List Bar) TranspositionState
    f acc n =
      let
        -- ( ns, s0 ) =  acc
        ns = fst acc
        s0 = snd acc

        -- ( n1, s1 ) =  transposeMusic s0 n
        result = transposeBar s0 n
        n1 = fst result
        s1 = snd result
      in
        -- ( n1 :: ns, s1 )
        Tuple ( n1 : ns ) s1
  in
    let
      -- ( tns, news ) = List.foldl f ( [], state ) ms
      res = foldl f (Tuple Nil state) bs
    in
      -- ( List.reverse tns, news )
      Tuple (reverse $ fst res) (snd res)

transposeBar :: TranspositionState -> Bar -> Tuple Bar TranspositionState
transposeBar state bar =
  let
    -- initialise accidentals list
    state0 = state  { sourceBarAccidentals = Accidentals.empty
                    , targetBarAccidentals = Accidentals.empty }
    (Tuple newMusic newState) = transposeMusicList state0 bar.music
    newBar = bar { music = newMusic }
  in
    Tuple newBar newState

transposeMusicList :: TranspositionState -> List Music -> Tuple (List Music) TranspositionState
transposeMusicList state ms =
  let
    f :: Tuple (List Music) TranspositionState -> Music -> Tuple (List Music) TranspositionState
    f acc n =
      let
        -- ( ns, s0 ) =  acc
        ns = fst acc
        s0 = snd acc

        -- ( n1, s1 ) =  transposeMusic s0 n
        result = transposeMusic s0 n
        n1 = fst result
        s1 = snd result
      in
        -- ( n1 :: ns, s1 )
        Tuple ( n1 : ns ) s1
  in
    let
      -- ( tns, news ) = List.foldl f ( [], state ) ms
      res = foldl f (Tuple Nil state) ms
    in
      -- ( List.reverse tns, news )
      Tuple (reverse $ fst res) (snd res)


transposeNoteList :: TranspositionState -> Nel.NonEmptyList AbcNote -> Tuple (Nel.NonEmptyList AbcNote) TranspositionState
transposeNoteList state nonemptyns =
  let
    ns =
      Nel.toList nonemptyns
    f :: Tuple (List AbcNote) TranspositionState -> AbcNote -> Tuple (List AbcNote) TranspositionState
    f acc n =
      let
        -- ( ns, s0 ) = acc
        n0 = fst acc
        s0 = snd acc

        -- ( n1, s1 ) = transposeNoteBy s0 n
        result = transposeNoteBy s0 n
        n1 = fst result
        s1 = snd result
      in
        -- ( n1 :: ns, s1 )
        Tuple (n1 : n0) s1
  in
    let
      -- ( tns, news ) = List.foldl f ( [], state ) ns
      res = foldl f (Tuple Nil state) ns
    in
      Tuple ((unsafeListToNel <<< reverse <<< fst) res) (snd res)



transposeRestOrNoteList :: TranspositionState -> Nel.NonEmptyList RestOrNote -> Tuple (Nel.NonEmptyList RestOrNote) TranspositionState
transposeRestOrNoteList state nonemptyns =
  let
    ns =
      Nel.toList nonemptyns
    f :: Tuple (List RestOrNote) TranspositionState -> RestOrNote -> Tuple (List RestOrNote) TranspositionState
    f acc rn =
      let
        -- ( ns, s0 ) = acc
        n0 = fst acc
        s0 = snd acc

        -- a rest (in a tuplet) doesn't affect the state, but a note does
        result =
          case rn of
            Left r ->
              Tuple (Left r) s0
            Right n ->
              lmap Right (transposeNoteBy s0 n)
        n1 = fst result
        s1 = snd result
      in
        -- ( n1 :: ns, s1 )
        Tuple (n1 : n0) s1
  in
    let
      -- ( tns, news ) = List.foldl f ( [], state ) ns
      res = foldl f (Tuple Nil state) ns
    in
      Tuple ((unsafeListToNel <<< reverse <<< fst) res) (snd res)

-- an unsafe conversion from a List to a NonEmptyList
-- use this when folding over NonEmptyLists and (for convenience)
-- you want to use a normal List fold and convert at each end
unsafeListToNel :: ∀ a. List a -> Nel.NonEmptyList a
unsafeListToNel xs =
  unsafePartial (go xs) where
    go :: Partial => List a -> Nel.NonEmptyList a
    go = fromJust <<< Nel.fromList

transposeChord :: TranspositionState -> AbcChord -> Tuple AbcChord TranspositionState
transposeChord state c =
  let
    result = transposeNoteList state c.notes
  in
    Tuple ( c { notes= fst result }) (snd result)

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
transposeNoteBy :: TranspositionState -> AbcNote -> Tuple AbcNote TranspositionState
transposeNoteBy state note =
  let
    -- make any implicit accidental explicit in the source note to be transposed if it's not marked as an accidental
    inSourceKeyAccidental =
      Accidentals.implicitInKeySet note.pitchClass (modifiedKeySet state.sourcemks)

    inSourceBarAccidental =
      lookup note.pitchClass state.sourceBarAccidentals

    -- we must do the lookup of the source accidental in this order - local bar overrides key
    maybeSourceAccidental =
      oneOf ( inSourceBarAccidental : inSourceKeyAccidental : Nil)

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
      note { pitchClass = safeKa.pitchClass
           , accidental = targetAcc
           , octave = note.octave + noteIdx.octaveIncrement }

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
      state { sourceBarAccidentals = newSourceAccs
            , targetBarAccidentals = newTargetAccs }
  in
    Tuple transposedNote newState


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
sharpNoteNumbers :: List ( Tuple Pitch Int )
sharpNoteNumbers =
  let
    f nn =
      let
        Pitch p = fst nn
        pos = snd nn
      in
        ((p.accidental == Sharp) && (p.pitchClass /= E && p.pitchClass /= B))
            || (p.accidental == Natural)
  in
    filter f pitchNumbers

-- | note pairs for the black and white notes of a piano,
-- | designating black notes with the Flat accidental
flatNoteNumbers :: List ( Tuple Pitch  Int )
flatNoteNumbers =
  let
    f nn =
      let
        Pitch p = fst nn
        pos = snd nn
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
    fromMaybe (Pitch  { pitchClass: C, accidental: Natural }) $ lookup i dict

-- | the inverted lookup for sharp chromatic scales.  This dictionaary
-- | allows you to enter a number (0 <= n < notesInChromaticScale) and return
-- | a (pitchClass, Accidental) pair which is the note's pitch
sharpNotedNumbers :: Map Int Pitch
sharpNotedNumbers =
  let
    invert (Tuple a b) =
      (Tuple  b a )
  in
    fromFoldable $ map invert sharpNoteNumbers

-- | the inverted lookup for flat chromatic scales.  This dictionaary
-- | allows you to enter a number (0 <= n < notesInChromaticScale) and return
-- | a (pitchClass, Accidental) pair which is the note's pitch
flatNotedNumbers :: Map Int Pitch
flatNotedNumbers =
  let
    invert (Tuple a b) =
      (Tuple  b a )
  in
    fromFoldable $ map invert flatNoteNumbers

-- | look up the note and return the number of its pitch in the range 0 <= n < notesInChromaticScale (0 is C Natural) -}
noteNumber :: AbcNote -> Int
noteNumber n =
  pitchNumber ( Pitch  { pitchClass: n.pitchClass, accidental: n.accidental })

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
transpositionDistance ::Pitch  -> Pitch  -> Int
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
        Key mks ->
          false

        _ ->
          true
    newhs =
      filter f hs
  in
    reverse $ ( Key newmks ) : (reverse newhs)

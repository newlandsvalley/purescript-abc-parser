module Music.Transposition
        ( defaultKey
        , keyDistance
        , transposeNote
        , transposeTo
        ) where



{- A parse tree score contains implicit accidentals.  Very often, the source text will not mark them but assume that they
   are implicit in the key signature.  They also may appear 'locally' - i.e. earlier in the bar and thus inherited.
   In order for the transposition process to work, all accidentals must be made explicit during transposition and then
   made implicit when written out to text if they are defined earlier in the bar or are defined in the key signature.

   This means we have to thread state through the transposition and hence use folds rather than maps
-}

import Prelude (($), (+), (-), (==), (/=), (&&), (||), (<), (<=), (>=), map, mod, negate)
import Data.Either (Either(..))
import Data.List (List(..), (:), filter, foldl, reverse)
import Data.Map (Map, fromFoldable, lookup)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Newtype (unwrap)
import Data.Foldable (oneOf)
import Abc.ParseTree
import Music.Accidentals as Accidentals
import Music.Notation (DiatonicScale, accidentalImplicitInKey, diatonicScale
                       , getKeySig, inScale
                       , isCOrSharpKey, modifiedKeySet, notesInChromaticScale
                       , transposeKeySignatureBy)

type TranspositionState =
    { keyDistance :: Int  -- semitone distance between keys - may be positive or negative
    , sourcemks :: ModifiedKeySignature -- source key signature
    , sourceBarAccidentals :: Accidentals.Accidentals -- any accidental defined locally to the current bar in the tune source
    , targetmks :: ModifiedKeySignature   -- target key signature
    , targetKeySet :: KeySet -- the set of accidental keys in the target key signature
    , targetScale :: DiatonicScale -- diatonic scale in the target key
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
  { keySignature: { pitchClass: C, accidental: Nothing, mode: Major }, modifications: Nil }

-- | Calculate the distance between the keys (target - source) measured in semitones.
-- | Keys must be in compatible modes.
keyDistance :: ModifiedKeySignature -> ModifiedKeySignature -> Either String Int
keyDistance targetmks srcmks =
  let
    target =
      targetmks.keySignature

    src =
      srcmks.keySignature

    targetAcc =
      explicitAccidental target.accidental

    srcAcc =
      explicitAccidental src.accidental
  in
    if (target.mode /= src.mode) then
      Left "incompatible modes"
    else
      Right (transpositionDistance
              ( KeyAccidental { pitchClass: target.pitchClass, accidental: targetAcc })
              ( KeyAccidental { pitchClass: src.pitchClass, accidental: srcAcc })
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


-- | Transpose a tune to the target key.
transposeTo :: ModifiedKeySignature -> AbcTune -> Either String AbcTune
transposeTo targetmks t =
  let
    -- get the key signature from the tune if there is one, default to C Major
    mks =
      fromMaybe defaultKey $ getKeySig t

    -- find the distance between the keys
    rdistance =
      keyDistance targetmks mks
  in
    case rdistance of
      Left e ->
        Left e

      Right d ->
        -- don't bother transposing if there's no distance between the keys
        if (d == 0) then
            Right t
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
            Right (transposeTune transpositionState t)

-- Implementation

transposeTune :: TranspositionState -> AbcTune -> AbcTune
transposeTune state t =
  let
    newHeaders =
      replaceKeyHeader state.targetmks t.headers
  in
    { headers: newHeaders, body: (transposeTuneBody state t.body) }



{- transpose the tune body.  We need to thread state through the tune in case there's an inline
   information header which changes key part way through the tune
-}
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
    Score ms ->
      let
        -- ( ms1, s1 ) = transposeMusicList state ms
        result = transposeMusicList state ms
        ms1 = fst result
        s1 = snd result
      in
        Tuple (Score ms1) s1

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
        result = transposeNoteList state ns
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
    Barline b ->
      Tuple ( Barline b)
          ( state  { sourceBarAccidentals = Accidentals.empty
                   , targetBarAccidentals = Accidentals.empty } )

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
        f :: forall m.Tuple (List m) TranspositionState -> m -> Tuple (List m) TranspositionState
        f acc n =
            let
                -- ( ns, s0 ) = acc
                ns = fst acc
                s0 = snd acc

                -- ( n1, s1 ) = transposeNoteBy s0 n
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


transposeNoteList :: TranspositionState -> List AbcNote -> Tuple (List AbcNote) TranspositionState
transposeNoteList state ns =
  let
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
      -- ( List.reverse tns, news )
      Tuple (reverse $ fst res) (snd res)



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
      accidentalImplicitInKey note.pitchClass (state.sourcemks)

    inSourceBarAccidental =
      lookup note.pitchClass state.sourceBarAccidentals

    -- we must do the lookup of the source accidental in this order - local bar overrides key
    implicitSourceAccidental =
      oneOf ( inSourceBarAccidental : inSourceKeyAccidental : Nil)

    explicitSourceNote =
      if (isJust note.accidental) then
        note
      else
        -- { note | accidental = implicitSourceAccidental }
        note { accidental = implicitSourceAccidental }

    srcNum =
      noteNumber explicitSourceNote

    -- ( targetNum, octaveIncrement ) =  noteIndex srcNum (state.keyDistance)
    noteIdx = noteIndex srcNum (state.keyDistance)

    ka =
      pitchFromInt (state.targetmks.keySignature) noteIdx.notePosition

    -- ( pc, acc ) = sharpenFlatEnharmonic ka
    safeKa = sharpenFlatEnharmonic ka

    --JMW
    targetBarAcc = Accidentals.lookup (unwrap safeKa).pitchClass state.targetBarAccidentals

    targetAcc =
      -- is it present in the local target bar accidentals
      if (Accidentals.member safeKa state.targetBarAccidentals) then
        Nothing
        -- is it present in the local target bar accidentals but with a different value
       -- else if (isJust (Accidentals.lookup safeKa.pitchClass state.targetBarAccidentals)) then
      else if (isJust targetBarAcc) then
        Just (unwrap safeKa).accidental
          -- is it in the set of keys in the target diatonic scale
      else if (inScale safeKa state.targetScale) then
        Nothing
      else
        Just (unwrap safeKa).accidental

    transposedNote =
      note { pitchClass = (unwrap safeKa).pitchClass
           , accidental = targetAcc
           , octave = note.octave + noteIdx.octaveIncrement }

    -- save any accidental nature of the original untransposed note
    newSourceAccs =
      addBarAccidental note.pitchClass note.accidental state.sourceBarAccidentals

    -- if the target, after all this, has an explicit accidental, we need to save it in the target bar accidental state
    newTargetAccs =
      if (isJust targetAcc) then
        -- we use the explicit form of the target
        addBarAccidental (unwrap safeKa).pitchClass (Just (unwrap safeKa).accidental) state.targetBarAccidentals
      else
        state.targetBarAccidentals

    -- update the state with both the source an target bar accidentals
    newState =
      state { sourceBarAccidentals = newSourceAccs
            , targetBarAccidentals = newTargetAccs }
  in
    Tuple transposedNote newState


{-| enharmonic equivalence for flattened accidentals
   We'll (for the moment) use the convention that most flat accidentals
   are presented in sharpened form
-}
sharpenFlatEnharmonic :: KeyAccidental -> KeyAccidental
sharpenFlatEnharmonic ka =
  case ka of
    KeyAccidental { pitchClass: G, accidental: Flat } ->
      KeyAccidental { pitchClass: F, accidental: Sharp }

    KeyAccidental { pitchClass: D, accidental: Flat } ->
      KeyAccidental { pitchClass: C, accidental: Sharp }

    _ ->
      ka

{- we need to take note of any accidentals so far in the bar because these may influence
   later notes in that bar.  If the note uses an accidental, add it to the accidental set.
-}
addBarAccidental :: PitchClass -> Maybe Accidental -> Accidentals.Accidentals -> Accidentals.Accidentals
addBarAccidental pc ma accs =
  case ma of
    Just acc ->
      Accidentals.add pc acc accs

    _ ->
      accs

{- create a list of pairs which should match every possible
   note pitch  (pitch class and accidental) with its offset into
   its 12-note chromatic scale
-}
noteNumbers :: List ( Tuple KeyAccidental Int )
noteNumbers =
  ( Tuple (KeyAccidental { pitchClass: C, accidental: Flat }) 11
  : Tuple (KeyAccidental { pitchClass: C, accidental: Natural }) 0
  : Tuple (KeyAccidental { pitchClass: C, accidental: Sharp }) 1
  : Tuple (KeyAccidental { pitchClass: C, accidental: DoubleSharp }) 2
  : Tuple (KeyAccidental { pitchClass: D, accidental: DoubleFlat }) 0
  : Tuple (KeyAccidental { pitchClass: D, accidental: Flat }) 1
  : Tuple (KeyAccidental { pitchClass: D, accidental: Natural }) 2
  : Tuple (KeyAccidental { pitchClass: D, accidental: Sharp }) 3
  : Tuple (KeyAccidental { pitchClass: D, accidental: DoubleSharp }) 4
  : Tuple (KeyAccidental { pitchClass: E, accidental: DoubleFlat }) 2
  : Tuple (KeyAccidental { pitchClass: E, accidental: Flat }) 3
  : Tuple (KeyAccidental { pitchClass: E, accidental: Natural }) 4
  : Tuple (KeyAccidental { pitchClass: E, accidental: Sharp }) 5
  : Tuple (KeyAccidental { pitchClass: E, accidental: DoubleSharp }) 6
  : Tuple (KeyAccidental { pitchClass: F, accidental: Flat }) 4
  : Tuple (KeyAccidental { pitchClass: F, accidental: Natural }) 5
  : Tuple (KeyAccidental { pitchClass: F, accidental: Sharp }) 6
  : Tuple (KeyAccidental { pitchClass: F, accidental: DoubleSharp }) 7
  : Tuple (KeyAccidental { pitchClass: G, accidental: DoubleFlat }) 5
  : Tuple (KeyAccidental { pitchClass: G, accidental: Flat }) 6
  : Tuple (KeyAccidental { pitchClass: G, accidental: Natural }) 7
  : Tuple (KeyAccidental { pitchClass: G, accidental: Sharp }) 8
  : Tuple (KeyAccidental { pitchClass: G, accidental: DoubleSharp }) 9
  : Tuple (KeyAccidental { pitchClass: A, accidental: DoubleFlat }) 7
  : Tuple (KeyAccidental { pitchClass: A, accidental: Flat }) 8
  : Tuple (KeyAccidental { pitchClass: A, accidental: Natural }) 9
  : Tuple (KeyAccidental { pitchClass: A, accidental: Sharp }) 10
  : Tuple (KeyAccidental { pitchClass: A, accidental: DoubleSharp }) 11
  : Tuple (KeyAccidental { pitchClass: B, accidental: DoubleFlat }) 9
  : Tuple (KeyAccidental { pitchClass: B, accidental: Flat }) 10
  : Tuple (KeyAccidental { pitchClass: B, accidental: Natural }) 11
  : Tuple (KeyAccidental { pitchClass: B, accidental: Sharp }) 0
  : Tuple (KeyAccidental { pitchClass: B, accidental: DoubleSharp }) 1
  : Nil
  )

{- note pairs for the black and white notes of a piano,
   designating black notes with the Sharp accidental
-}
sharpNoteNumbers :: List ( Tuple KeyAccidental Int )
sharpNoteNumbers =
  let
    f nn =
      let
        ka = unwrap $ fst nn
        pos = snd nn
      in
        ((ka.accidental == Sharp) && (ka.pitchClass /= E && ka.pitchClass /= B))
            || (ka.accidental == Natural)
  in
    filter f noteNumbers

{- note pairs for the black and white notes of a piano,
   designating black notes with the Flat accidental
-}
flatNoteNumbers :: List ( Tuple KeyAccidental Int )
flatNoteNumbers =
  let
    f nn =
      let
        ka = unwrap $ fst nn
        pos = snd nn
      in
        ((ka.accidental == Flat) && (ka.pitchClass /= F && ka.pitchClass /= C))
          || (ka.accidental == Natural)
  in
    filter f noteNumbers



{- given a key signature and an integer (0 <= n < notesInChromaticScale)
   return the pitch of the note within that signature
-}
pitchFromInt :: KeySignature -> Int -> KeyAccidental
pitchFromInt ks i =
  let
    dict =
      if (isCOrSharpKey ks) then
        sharpNotedNumbers
      else
        flatNotedNumbers
  in
    fromMaybe (KeyAccidental { pitchClass: C, accidental: Natural }) $ lookup i dict



{- the inverted lookup for sharp chromatic scales.  This dictionaary
   allows you to enter a number (0 <= n < notesInChromaticScale) and return
   a (pitchClass, Accidental) pair which is the note's pitch
-}
sharpNotedNumbers :: Map Int KeyAccidental
sharpNotedNumbers =
  let
    invert (Tuple a b) =
      (Tuple  b a )
  in
    fromFoldable $ map invert sharpNoteNumbers



{- the inverted lookup for flat chromatic scales.  This dictionaary
   allows you to enter a number (0 <= n < notesInChromaticScale) and return
   a (pitchClass, Accidental) pair which is the note's pitch
-}
flatNotedNumbers :: Map Int KeyAccidental
flatNotedNumbers =
  let
    invert (Tuple a b) =
      (Tuple  b a )
  in
    fromFoldable $ map invert flatNoteNumbers


{- a dictionary of comparable note -> note number -}
chromaticScaleDict :: Map KeyAccidental Int
chromaticScaleDict =
  fromFoldable noteNumbers


lookupChromatic :: Map KeyAccidental Int -> KeyAccidental -> Int
lookupChromatic dict target =
  fromMaybe 0 $ lookup target dict



{- look up the pitch and return a number in the range 0 <= n < notesInChromaticScale  (0 is C Natural) -}


pitchNumber :: KeyAccidental -> Int
pitchNumber ka =
  lookupChromatic chromaticScaleDict ka


explicitAccidental :: Maybe Accidental -> Accidental
explicitAccidental ma =
  fromMaybe Natural ma

{- look up the note and return the number of its pitch in the range 0 <= n < notesInChromaticScale (0 is C Natural) -}
noteNumber :: AbcNote -> Int
noteNumber n =
  let
    acc = explicitAccidental n.accidental
  in
    pitchNumber ( KeyAccidental { pitchClass: n.pitchClass, accidental: acc })

{- inspect the current note index and the amount it is to be incremented by.
   produce a new note index in the range (0 <= n < notesInChromaticScale)
   and associate with this a number (-1,0,1) which indicates an increment to the octave
-}
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



{- work out the minimum transposition distance (target - source) or (source - target) measured in semitones -}
transpositionDistance :: KeyAccidental -> KeyAccidental -> Int
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



{- replace a Key header (if it exists) -}
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
    reverse $ ( Key newmks ) : newhs

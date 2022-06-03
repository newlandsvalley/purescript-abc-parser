# ABC Parser Guide

ABC is a notation for describing a traditional tune such that all aspects of it can be written down in a textual format. This means that to get started, the only tooling you require is an ordinary text editor. However it is highly advisable to use one that is unicode-aware, particularly if you are transcribing tunes whose titles, descriptions and so forth use different alphabets.  

You will first need to learn the basics of the notation.  There are good tutorials available - for example [The Lesession pages](http://www.lesession.co.uk/abc/abc_notation.htm) by Steve Mansfield or else this [Interactive tutorial](http://www.tradtunedb.org.uk/#/tutorial). There are also various online collections of tunes in ABC format and you can alternatively download your tune from one of these.  Particularly recommended are [The Session](https://thesession.org/) for Irish tunes or [FolkWiki](http://www.folkwiki.se/) for Swedish ones.

## Installing Dependencies

For the initial examples, the following dependencies are required:

```
  dependencies =
  [ "abc-parser"
  , "aff"
  , "console"
  , "effect"
  , "either"
  , "maybe"
  , "node-buffer"
  , "node-fs-aff"
  , "node-path"
  , "prelude"
  ]
```

## Basic Parsing

You therefore probably start with a text file which is a representation of your tune in ABC format. Let's suppose it's called ```mytune.abc``` in a subdirectory named ```abc```. In order to do anything with it after reading it, you must first parse it into a data structure which is either of type ```Right AbcTune``` if it conforms to the notation specification or ```Left ParseError``` if it does not.  In this example, we read the file using facilities in ```node-fs-aff```, parse the contents and then log the tune title.

```purs
module Main where

import Prelude

import Data.Abc.Metadata (getTitle)
import Data.Abc.Parser (parse)
import Data.Either (Either(..))
import Data.Maybe (fromMaybe, maybe)
import Effect (Effect)
import Effect.Console (log)
import Effect.Class (liftEffect)
import Effect.Aff (Aff, Fiber, launchAff)
import Node.Path as Path
import Node.Buffer (toString)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readFile)

main :: Effect (Fiber Unit)
main = launchAff do
  tuneString <- readAbcFile "abc" "mytune.abc"
  case parse tuneString of 
    Right tune -> do
      liftEffect $ log ("tune title is " <> (fromMaybe "untitled" $ getTitle tune))
    Left err -> do
      liftEffect $ log ("parse failed: " <> show err)


-- | read the ABC file
readAbcFile :: String -> String -> Aff String
readAbcFile directory fileName =
  do
    buffer <- readFile (Path.concat [directory, fileName])
    contents <- liftEffect $ (toString UTF8) buffer
    pure contents
```

## Changing the Octave

If you need to alter the pitch of the tune by an octave (either up or down), you can do so quite easily. You must additionally import the ```Octave``` module and also it is useful to import the ```Canonical``` module which converts an ```AbcTune``` back to a string with regular whitespace:

```purs
import Data.Abc.Canonical (fromTune)
import Data.Abc.Octave (up) as Octave
```

You may then convert the (successfully parsed) tune and redisplay it. You will see that each note in the body of the tune is an octave higher (for example ```A``` becames ```a```).

```purs
  case parse tuneString of 
    Right tune -> do 
      liftEffect $ log ("new tune is " <> (fromTune $ Octave.up tune))
```

## Transposition
Transposing the tune to a new key is similar. You first import the ```Transposition``` module, and also you need types from the ```Abc``` module in order to describe the new key:

```purs
import Data.Abc (Accidental(..), Pitch(..), PitchClass(..))
import Data.Abc.Transposition (transposeTo)
```

To transpose, you simply need to provide the new pitch.  The mode of the original tune remains unaltered.  For example, suppose our original tune was in the key of ```G Major``` and we want to tranapose it to ```D Major```:

```purs
  case parse tuneString of 
    Right tune -> do 
      let 
        newPitch = Pitch { pitchClass: D, accidental: Natural }
      liftEffect $ log ("transposed tune is " <> (fromTune $ transposeTo newPitch tune))
```

You will see that the pitch of each note has changed and also the ```K: - Key Signature``` header has been changed to the new key.

## Accessing the Tune Headers

ABC is structured as a set of headers which contain metadata (title, key signature, tempo etc.) followed by the body which holds the actual notes. ```abc-parser``` comes equipped with a set of ```profunctor optics``` which help you to fetch or edit the metadata. As an example, suppose you want to find the ```mode``` of the tune. You will need to add ```profunctor-lenses``` to your spago dependencies and then you can import some lens functions:

```pure

import Data.Abc.Optics (_headers, _ModifiedKeySignature, _keySignature, _mode)
import Data.Lens.Fold (firstOf)
import Data.Lens.Traversal (traversed)
```

You can then display the mode:

```purs
  case parse tuneString of 
    Right tune -> do
      let 
        mode = firstOf
          (_headers <<< traversed <<< _ModifiedKeySignature <<< _keySignature <<< _mode) tune
      liftEffect $ log ("mode is " <> (maybe "unknown" show mode))
```

(The mode will be unknown if there happens to be no Key Signature header present.)

There are some convenience functions for accessing the more common headers in the ```Metadata``` module such as the ```getTitle``` function we used earlier but you could equally well use the optic if you prefer (```_Title``` in this case). These functions are implemented in terms of the underlying optic.

## Generating a MIDI Recording

You can generate a MIDI file from the ABC tune.  You will have to add to your dependencies ```midi``` and also some mechanism for saving the binary file - this example uses James D Brock's ```arraybuffer-builder``` and you also need ```foldable-traversable```. The complete code is:

```purs
module Main where

import Prelude

import Data.Abc.Parser (parse)
import Data.Abc.Midi (toMidi)
import Data.Array (fromFoldable) as A
import Data.Midi as Midi
import Data.List (List)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Aff (Aff, Fiber, launchAff)
import Node.Path as Path
import Node.Buffer (toString, fromArrayBuffer)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readFile, writeFile)
import Data.ArrayBuffer.Builder (PutM, execPut, putInt8)
import Data.Foldable (traverse_)

main :: Effect (Fiber Unit)
main = launchAff do
  tuneString <- readAbcFile "abc" "mytune.abc"
  case parse tuneString of 
    Right tune -> do
      let    
        midi = toMidi tune 
      writeMidiFile midi
    Left err -> do
      liftEffect $ log ("parse failed: " <> show err)


-- | read the ABC file
readAbcFile :: String -> String -> Aff String
readAbcFile directory fileName =
  do
    buffer <- readFile (Path.concat [directory, fileName])
    contents <- liftEffect $ (toString UTF8) buffer
    pure contents

-- | write the raw MIDI file
writeMidiFile :: List Midi.Byte -> Aff Unit 
writeMidiFile midi = 
  do
    arrayBuffer <- liftEffect $ execPut $ putArrayInt8 (A.fromFoldable midi)
    nodeBuffer <- liftEffect $ fromArrayBuffer arrayBuffer
    writeFile "mytune.mid" nodeBuffer

putArrayInt8 :: forall m. MonadEffect m => Array Midi.Byte -> PutM m Unit
putArrayInt8 xs = do
  traverse_ putInt8 xs
```

You will now be able to load the MIDI file into your favourite MIDI synth and listen to it.

## More Facilities for use in the Browser

If you are producing code for the browser, and in particular, if you are using [Halogen](https://github.com/purescript-halogen/purescript-halogen) for your UI, then there are further facilities you can use.

You can generate a score from the tune using [purescript-abc-scores](https://github.com/newlandsvalley/purescript-abc-scores).  If you would like to play the tune directly, then [purescript-abc-melody](https://github.com/newlandsvalley/purescript-abc-melody) will generate a melody which can then be played using the player widget from [purescript-halogen-components](https://github.com/newlandsvalley/purescript-halogen-components).
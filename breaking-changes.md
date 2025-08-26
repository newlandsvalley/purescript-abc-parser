Breaking Changes in v2.1.0
--------------------------

  * PureScript v0.15.15
  * Build with spago@next  

Breaking Changes in v2.0.1
--------------------------

  * PureScript v0.15.9
  * There is now a transitive dependency on ```purescript-js-bigints``` via the latest ```purescript-rationals```

Breaking Changes in v2.0.0
--------------------------

  * PureScript v0.15.4
  * Header optics have been added; profunctor-lenses is now a dependency
  * Some metadata header retrieval functions have been removed in favour of optics
  * ```MeterSignature``` - ```Tuple Int Int``` replaced by 
    ```TimeSignature``` - ```{ numerator:: Int, denominator:: Int }```
  * MIDI pitch translation for B# and B## has been corrected
  * Decorations can now be applied to chords and tuplets
  * Chord symbols appear in the ADT as simple strings (without framing double quotes)
  * The ```Metadata``` module has been renamed as ```Utils``` and many functions moved
    to modules named after the appropriate header

Breaking Changes in v1.9.3
--------------------------

  * The definition of __Chord__ has been altered to allow for decorations and slurs 
  * A __Tuplet__ type has been introduced

Breaking Changes in v1.9.2
--------------------------

  * The definition of __Volta__ has changed in order to allow for volta ranges

Breaking Changes in v1.9.0
--------------------------  

  * The __Repeat__ sum type in the parse tree ADT has been removed.  The __BarType__ record type has been replaced by the (simpler) __BarLine__.
  * The representation of begin and end repeats at a bar line has therefore changed.
  * The signature of __partitionTuneBody__ in the Voice module has been altered.
  * A new data type, __Volta__ has been introduced into the parse tree ADT.  
  This is so as to allow more comple voltas of the form |1,2 to be represented.
  This, therefore replaces the simpler Int data type we had before.
  * Optional properties (as key-value pairs) added to the __Key__ header.
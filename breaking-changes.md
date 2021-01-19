Breaking Changes in this branch
===============================

* The __Repeat__ sum type in the parse tree ADT has been removed.  The __BarType__ record type has been replaced by the (simpler) __BarLine__.
* The representation of begin and end repeats at a bar line has therefore changed.
* The signature of __partitionTuneBody__ in the Voice module has been altered.
* A new data type, __Volta__ has been introduced into the parse tree ADT.  
  This is so as to allow more comple voltas of the form |1,2 to be represented.
  This, therefore replaces the simpler Int data type we had before.
module Fathom.Data.Sing


||| A singleton type, constrained to be a single value
|||
||| The underlying value and the proof are both erased at runtime, as they can
||| be converted back to the index by reconstructing the value as required.
|||
||| Inspired by the singleton type [found in Adga’s documentation](https://agda.readthedocs.io/en/v2.5.4.1/language/with-abstraction.html#the-inspect-idiom).
public export
record Sing {0 A : Type} (x : A) where
  constructor MkSing
  ||| The underlying value of the singleton (erased at run-time)
  0 val : A
  ||| A proof that @val is the same as the indexed value (erased at run-time)
  {auto 0 prf : x = val}


||| Convert a singleton back to its underlying value restoring it with a value
||| constructed runtime
public export
val : {0 A : Type} -> {x : A} -> Sing x -> A
val (MkSing _) = x

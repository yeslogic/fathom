module Fathom.Data.Sing


||| A type constrained to a single value
|||
||| The underlying value is erased at runtime, as it can be converted back to
||| the index by reconstructing the value as required.
public export
data Sing : {0 A : Type} -> (x : A) -> Type where
  MkSing : {0 A : Type} -> (0 x : A) -> Sing x


||| Reconstruct a singleton with a runtime value.
public export
val : {0 A : Type} -> {x : A} -> Sing x -> A
val (MkSing _) = x


||| Update the value contained in a singleton with a function.
export
map : {0 A, B : Type} -> {0 x : A} -> (f : A -> B) -> Sing x -> Sing (f x)
map f (MkSing y) = MkSing (f y)



namespace SingEq
  -- NOTE: Unsure if this representation is actually needed?

  ||| A type constrained to be a single value, with an associated equality proof.
  |||
  ||| The underlying value and the proof are both erased at runtime, as they can
  ||| be converted back to the index by reconstructing the value as required.
  |||
  ||| Inspired by the singleton type [found in Adga’s documentation](https://agda.readthedocs.io/en/v2.5.4.1/language/with-abstraction.html#the-inspect-idiom).
  public export
  record SingEq {0 A : Type} (x : A) where
    constructor MkSingEq
    ||| The underlying value of the singleton (erased at run-time)
    0 val : A
    ||| A proof that @val is the same as the indexed value (erased at run-time)
    {auto 0 prf : x = val}


  ||| Convert a singleton back to its underlying value restoring it with a value
  ||| constructed runtime
  public export
  val : {0 A : Type} -> {x : A} -> SingEq x -> A
  val (MkSingEq _) = x


  ||| Update the value contained in a singleton with a function.
  export
  map : {0 A, B : Type} -> {0 x : A} -> (f : A -> B) -> SingEq x -> SingEq (f x)
  map f (MkSingEq y {prf}) = MkSingEq (f y) {prf = cong f prf}


withEq : {0 A : Type} -> {0 x : A} -> Sing x -> SingEq x
withEq (MkSing x) = MkSingEq x

withoutEq : {0 A : Type} -> {0 x : A} -> SingEq x -> Sing x
withoutEq {x} (MkSingEq _) = MkSing x

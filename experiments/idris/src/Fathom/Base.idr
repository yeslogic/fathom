module Fathom.Base


import Data.Colist
import Data.List


------------------
-- USEFUL TYPES --
------------------


||| A value that is refined by a proposition.
|||
||| This is a bit like `(x : A ** B)`, but with the second element erased.
public export
record Refine (0 A : Type) (0 P : A -> Type) where
  constructor MkRefine
  ||| The wrapped value
  value : A
  ||| The proof of the proposition
  {auto 0 prf : P value}

||| Refine a value with a proposition
public export
refine : {0 A : Type} -> {0 P : A -> Type} -> (value : A) -> {auto 0 prf : P value} -> Refine A P
refine value = MkRefine { value }


||| Singleton types
|||
||| Inspired by [this type](https://agda.readthedocs.io/en/v2.5.4.1/language/with-abstraction.html#the-inspect-idiom)
||| from the Agda docs.
public export
record Sing {0 A : Type} (x : A) where
  constructor MkSing
  0 value : A
  {auto 0 prf : x = value}


||| Convert a singleton back to its underlying value
public export
sing : {0 A : Type} -> {0 x : A} -> (0 value : A) -> {auto 0 prf : x = value} -> Sing x
sing value = MkSing { value }

||| Convert a singleton back to its underlying value
public export
value : {0 Val : Type} -> {x : Val} -> Sing x -> Val
value (MkSing _) = x


---------------------------
-- ENCODER/DECODER PAIRS --
---------------------------

-- Inspiration taken from Narcissus:
--
-- * [Narcissus: Correct-by-Construction Derivation of Decoders and Encoders from Binary Formats](https://dl.acm.org/doi/10.1145/3341686)
--   by Delaware et. al.
-- * [`Narcissus/Common/Specs.v`](https://github.com/mit-plv/fiat/blob/master/src/Narcissus/Common/Specs.v)
--
-- TODO: Add support for [Narcissus-style stores](https://github.com/mit-plv/fiat/tree/master/src/Narcissus/Stores)

parameters (Source : Type, Target : Type)

  ||| Decoders consume a _target value_ and produce either:
  |||
  ||| - a _source value_ and _remaining target value_
  ||| - or nothing if in error occurred
  |||
  ||| @ Source  The type of source values (usually an in-memory data structure)
  ||| @ Target  The type of target values (usually a byte-stream)
  public export
  Decode : Type
  Decode = Target -> Maybe (Source, Target)

  ||| Encoders take a _source value_ and _remaining target value_ and produce either:
  |||
  ||| - an _updated target value_
  ||| - or nothing if in error occurred
  |||
  ||| @ Source  The type of source values (usually an in-memory data structure)
  ||| @ Target  The type of target values (usually a byte-stream)
  public export
  Encode : Type
  Encode = Source -> Target -> Maybe Target


----------------------
-- ENCODING TARGETS --
----------------------


||| A possibly infinite stream of bits
public export
BitStream : Type
BitStream = Colist Bool


||| A possibly infinite stream of bytes
public export
ByteStream : Type
ByteStream = Colist Bits8


||| A finite bit buffer
public export
BitBuffer : Type
BitBuffer = List Bool


||| A finite byte buffer
public export
ByteBuffer : Type
ByteBuffer = List Bits8

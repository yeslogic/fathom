module Fathom.Base


import Data.Colist
import Data.List


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

%name BitStream stream


||| A possibly infinite stream of bytes
public export
ByteStream : Type
ByteStream = Colist Bits8

%name ByteStream stream


||| A finite bit buffer
public export
BitBuffer : Type
BitBuffer = List Bool

%name BitBuffer buffer


||| A finite byte buffer
public export
ByteBuffer : Type
ByteBuffer = List Bits8

%name ByteBuffer buffer

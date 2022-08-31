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

parameters (Source, Target : Type)

  ||| Decoders consume a _target value_ and produce either:
  |||
  ||| - a _source value_
  ||| - or nothing if in error occurred
  |||
  ||| @ Source  The type of source values (usually an in-memory data structure)
  ||| @ Target  The type of target values (usually a byte-stream)
  public export
  Decode : Type
  Decode = Target -> Maybe Source


  ||| Encoders take a _source value_ and produce either:
  |||
  ||| - a _target value_
  ||| - or nothing if in error occurred
  |||
  ||| @ Source  The type of source values (usually an in-memory data structure)
  ||| @ Target  The type of target values (usually a byte-stream)
  public export
  Encode : Type
  Encode = Source -> Maybe Target


parameters (Source, Target : Type)

  ||| Decode a portion of a _target value_, leaving some remaining for
  ||| subsequent decoding.
  |||
  ||| @ Source  The type of source values (usually an in-memory data structure)
  ||| @ Target  The type of target values (usually a byte-stream)
  public export
  DecodePart : Type
  DecodePart = Decode (Source, Target) Target


  ||| Consumes a _source value_ and the remaining _target value_, returning
  ||| a fully encoded target value.
  |||
  ||| @ Source  The type of source values (usually an in-memory data structure)
  ||| @ Target  The type of target values (usually a byte-stream)
  public export
  EncodePart : Type
  EncodePart = Encode (Source, Target) Target


parameters {0 Source, Target : Type}

  public export
  toDecodeFull : (Monoid Target, Eq Target) => DecodePart Source Target -> Decode Source Target
  toDecodeFull decode target = do
    (source, target') <- decode target
    if target == neutral then Just source else Nothing


  public export
  toEncodeFull : Monoid Target => EncodePart Source Target -> Encode Source Target
  toEncodeFull encode source = encode (source, neutral)


  public export
  toEncodePart : Monoid Target => Encode Source Target -> EncodePart Source Target
  toEncodePart encode (source, target) = [| encode source <+> Just target |]


----------------------
-- ENCODING TARGETS --
----------------------


||| A potentially infinite stream of bits
public export
BitStream : Type
BitStream = Colist Bool

%name BitStream bits


||| A potentially infinite stream of bytes
public export
ByteStream : Type
ByteStream = Colist Bits8

%name ByteStream bytes


||| A finite bit buffer
public export
BitBuffer : Type
BitBuffer = List Bool

%name BitBuffer bits


||| A finite byte buffer
public export
ByteBuffer : Type
ByteBuffer = List Bits8

%name ByteBuffer bytes


||| An array of bits of a known size
public export
BitArray : Nat -> Type
BitArray len = Vect len Bool

%name BitArray bits


||| An array of bytes of a known size
public export
ByteArray : Nat -> Type
ByteArray len = Vect len Bits8

%name ByteArray bytes

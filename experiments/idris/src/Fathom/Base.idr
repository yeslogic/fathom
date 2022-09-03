module Fathom.Base


import Data.Bits
import Data.Colist
import Data.List
import Data.Vect


||| Return the type of an expression, without consuming it
public export
typeOf : {1 A : Type} -> (0 x : A) -> Type
typeOf _ = A


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
  toDecodeFull decode target = Prelude.do
    (source, target') <- decode target
    if target == neutral then Just source else Nothing


  public export
  toEncodeFull : Monoid Target => EncodePart Source Target -> Encode Source Target
  toEncodeFull encode source = encode (source, neutral)


  public export
  toEncodePart : Monoid Target => Encode Source Target -> EncodePart Source Target
  toEncodePart encode (source, target) = [| encode source <+> Just target |]


namespace DecodePart

  -- TODO: Should probably implement functor, applicative, or monad here. or use
  -- the reader, writer or state monad transformers

  public export
  map : {0 S1, S2, T : Type} -> (S1 -> S2) -> DecodePart S1 T -> DecodePart S2 T
  map f decode target =
    Prelude.map (\(source, target') => (f source, target)) (decode target)


  public export
  pure : {0 S, T : Type} -> S -> DecodePart S T
  pure source target = Just (source, target)


  public export
  (<*>) : {0 S1, S2, T : Type} -> DecodePart (S1 -> S2) T -> DecodePart S1 T -> DecodePart S2 T
  (<*>) decodeFun decode target = do
    (fun, target1) <- decodeFun target
    (source, target2) <- decode target1
    Just (fun source, target2)


  public export
  ignore : {0 S, T : Type} -> DecodePart S T -> DecodePart () T
  ignore = map (const ())


  public export
  bind : {0 S1, S2, T : Type} -> DecodePart S1 T -> (S1 -> DecodePart S2 T) -> DecodePart S2 T
  bind decode1 decode2 target = do
    (source1, target') <- decode1 target
    decode2 source1 target'


  public export
  (>>=) : {0 S1, S2, T : Type} -> DecodePart S1 T -> (S1 -> DecodePart S2 T) -> DecodePart S2 T
  (>>=) = bind


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


||| The byte order of some encoded data, usually a number.
public export
data ByteOrder : Type where
  LE : ByteOrder
  BE : ByteOrder


namespace ByteStream

  export
  decodeU8 : DecodePart Bits8 ByteStream
  decodeU8 [] = Nothing
  decodeU8 (x :: bytes) =  Just (x, bytes)


  export
  encodeU8 : Encode Bits8 ByteStream
  encodeU8 x = Just [x]


  export
  decodeU16 : ByteOrder -> DecodePart Bits16 ByteStream
  decodeU16 LE = DecodePart.do
    b0 <- map (cast {to = Bits16}) decodeU8
    b1 <- map (cast {to = Bits16}) decodeU8
    pure (b0 .|. b1 `shiftL` fromNat 8)
  decodeU16 BE = DecodePart.do
    b0 <- map (cast {to = Bits16}) decodeU8
    b1 <- map (cast {to = Bits16}) decodeU8
    pure (b0 `shiftL` fromNat 8 .|. b1)


  export
  encodeU16 : ByteOrder -> Encode Bits16 ByteStream
  encodeU16 LE x = Just [cast x, cast (x `shiftR` fromNat 8)]
  encodeU16 BE x = Just [cast (x `shiftR` fromNat 8), cast x]


  export
  decodeU32 : ByteOrder -> DecodePart Bits32 ByteStream
  decodeU32 LE = DecodePart.do
    b0 <- map (cast {to = Bits32}) decodeU8
    b1 <- map (cast {to = Bits32}) decodeU8
    b2 <- map (cast {to = Bits32}) decodeU8
    b3 <- map (cast {to = Bits32}) decodeU8
    pure (b0 .|. b1 `shiftL` fromNat 8 .|. b2 `shiftL` fromNat 16 .|. b2 `shiftL` fromNat 24)
  decodeU32 BE = DecodePart.do
    b0 <- map (cast {to = Bits32}) decodeU8
    b1 <- map (cast {to = Bits32}) decodeU8
    b2 <- map (cast {to = Bits32}) decodeU8
    b3 <- map (cast {to = Bits32}) decodeU8
    pure (b0 `shiftL` fromNat 24 .|. b1 `shiftL` fromNat 16 .|. b2 `shiftL` fromNat 8 .|. b3)


  export
  encodeU32 : ByteOrder -> Encode Bits32 ByteStream
  encodeU32 LE x = Just [cast x, cast (x `shiftR` fromNat 8), cast (x `shiftR` fromNat 16), cast (x `shiftR` fromNat 24)]
  encodeU32 BE x = Just [cast (x `shiftR` fromNat 24), cast (x `shiftR` fromNat 16), cast (x `shiftR` fromNat 8), cast x]


  -- decodeU : Bits a => ByteOrder -> DecodePart a ByteStream
  -- encodeU : Bits a => ByteOrder -> Encode a ByteStream

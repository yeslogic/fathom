||| Open format universe
|||
||| This module defines an open universe of binary format descriptions using
||| records to define an interface. By defining formats in this way, the
||| universe of formats is open to extension.
|||
||| I’m not sure, but this reminds me a little of the ‘coinductively defined
||| universes’ that [some type theorists were proposing](https://www.cmu.edu/dietrich/philosophy/hott/slides/shulman-2022-05-12.pdf#page=79),
||| but I may be mistaken.

module Fathom.Open.Record


import Data.Colist
import Data.Vect

import Fathom.Base
import Fathom.Data.Sing


-------------------------
-- FORMAT DESCRIPTIONS --
-------------------------


public export
record Format where
  constructor MkFormat
  Rep : Type
  decode : Decode (Rep, ByteStream) ByteStream
  encode : Encode Rep ByteStream


---------------------------------
-- INDEXED FORMAT DESCRIPTIONS --
---------------------------------


||| A format description refined with a fixed representation
public export
data FormatOf : (0 A : Type) -> Type where
  MkFormatOf : (f : Format) -> FormatOf f.Rep


------------------------------------
-- FORMAT DESCRIPTION CONVERSIONS --
------------------------------------


public export
toFormatOf : (f : Format) -> FormatOf f.Rep
toFormatOf f = MkFormatOf f


public export
toFormat : {0 A : Type} -> FormatOf A -> Format
toFormat (MkFormatOf f) = f


public export
toFormatOfEq : {0 A : Type} -> (f : Format ** f.Rep = A) -> FormatOf A
toFormatOfEq (f ** prf) = rewrite sym prf in MkFormatOf f


public export
toFormatEq : {0 A : Type} -> FormatOf A -> (f : Format ** f.Rep = A)
toFormatEq (MkFormatOf f) = (f ** Refl)


--------------
-- FORMATS --
--------------


public export
end : Format
end = MkFormat { Rep, decode, encode } where
  Rep : Type
  Rep = Unit

  decode : Decode (Rep, ByteStream) ByteStream
  decode [] = Just ((), [])
  decode (_::_) = Nothing

  encode : Encode Rep ByteStream
  encode () = Just []


public export
fail : Format
fail = MkFormat { Rep, decode, encode } where
  Rep : Type
  Rep = Void

  decode : Decode (Rep, ByteStream) ByteStream
  decode _ = Nothing

  encode : Encode Rep ByteStream
  encode x = void x


public export
pure : {0 A : Type} -> A -> Format
pure x = MkFormat { Rep, decode, encode } where
  Rep : Type
  Rep = Sing x

  decode : Decode (Rep, ByteStream) ByteStream
  decode buffer = Just (MkSing x, buffer)

  encode : Encode Rep ByteStream
  encode (MkSing _) = Just []


public export
skip : (f : Format) -> (def : f.Rep) -> Format
skip f def = MkFormat { Rep, decode, encode } where
  Rep : Type
  Rep = ()

  decode : Decode (Rep, ByteStream) ByteStream
  decode buffer = do
    (x, buffer') <- f.decode buffer
    Just ((), buffer')

  encode : Encode Rep ByteStream
  encode () = f.encode def


public export
repeat : Nat -> Format -> Format
repeat len f = MkFormat { Rep, decode, encode } where
  Rep : Type
  Rep = Vect len f.Rep

  decode : Decode (Rep, ByteStream) ByteStream
  decode = go len where
    go : (len : Nat) -> Decode (Vect len f.Rep, ByteStream) ByteStream
    go 0 buffer = Just ([], buffer)
    go (S len) buffer = do
      (x, buffer') <- f.decode buffer
      (xs, buffer'') <- go len buffer'
      Just (x :: xs, buffer'')

  encode : Encode Rep ByteStream
  encode = go len where
    go : (len : Nat) -> Encode (Vect len f.Rep) ByteStream
    go 0 [] = Just []
    go (S len) (x :: xs) =
      [| f.encode x <+> go len xs |]


public export
bind : (f : Format) -> (f.Rep -> Format) -> Format
bind f1 f2 = MkFormat { Rep, decode, encode } where
  Rep : Type
  Rep = (x : f1.Rep ** (f2 x).Rep)

  decode : Decode (Rep, ByteStream) ByteStream
  decode buffer = do
    (x, buffer') <- f1.decode buffer
    (y, buffer'') <- (f2 x).decode buffer'
    Just ((x ** y), buffer'')

  encode : Encode Rep ByteStream
  encode (x ** y) =
    [| f1.encode x <+> (f2 x).encode y |]


-- Support for do notation

public export
(>>=) : (f : Format) -> (Rep f -> Format) -> Format
(>>=) = bind


--------------------
-- CUSTOM FORMATS --
--------------------


public export
u8 : Format
u8 = MkFormat
  { Rep = Bits8
  , decode = decodeU8
  , encode = encodeU8
  }


public export
u16Le : Format
u16Le = MkFormat
  { Rep = Bits16
  , decode = decodeU16 LE
  , encode = encodeU16 LE
  }


public export
u16Be : Format
u16Be = MkFormat
  { Rep = Bits16
  , decode = decodeU16 BE
  , encode = encodeU16 BE
  }

||| Open format universe
|||
||| This module defines an open universe of binary format descriptions using
||| records to define an interface. By defining formats in this way, the
||| universe of formats is open to extension.
|||
||| I’m not sure, but this reminds me a little of the ‘coinductively defined
||| universes’ that [some type theorists were proposing](https://www.cmu.edu/dietrich/philosophy/hott/slides/shulman-2022-05-12.pdf#page=79),
||| but I may be mistaken.

module Fathom.Format.Record


import Data.Colist
import Data.DPair
import Data.HVect
import Data.Vect

import Fathom.Base
import Fathom.Data.Iso
import Fathom.Data.Sing


-------------------------
-- FORMAT DESCRIPTIONS --
-------------------------


public export
record Format where
  constructor MkFormat
  Rep : Type
  decode : DecodePart Rep ByteStream
  encode : Encode Rep ByteStream


namespace Format

  public export
  end : Format
  end = MkFormat { Rep, decode, encode } where
    Rep : Type
    Rep = Unit

    decode : DecodePart Rep ByteStream
    decode [] = Just ((), [])
    decode (_::_) = Nothing

    encode : Encode Rep ByteStream
    encode () = Just []


  public export
  fail : Format
  fail = MkFormat { Rep, decode, encode } where
    Rep : Type
    Rep = Void

    decode : DecodePart Rep ByteStream
    decode = const Nothing

    encode : Encode Rep ByteStream
    encode x = void x


  public export
  pure : {0 A : Type} -> A -> Format
  pure x = MkFormat { Rep, decode, encode } where
    Rep : Type
    Rep = Sing x

    decode : DecodePart Rep ByteStream
    decode = pure (MkSing x)

    encode : Encode Rep ByteStream
    encode (MkSing _) = pure []


  public export
  ignore : (f : Format) -> (def : f.Rep) -> Format
  ignore f def = MkFormat { Rep, decode, encode } where
    Rep : Type
    Rep = ()

    decode : DecodePart Rep ByteStream
    decode = ignore f.decode

    encode : Encode Rep ByteStream
    encode () = f.encode def


  public export
  repeat : Nat -> Format -> Format
  repeat len f = MkFormat { Rep, decode, encode } where
    Rep : Type
    Rep = Vect len f.Rep

    decode : DecodePart Rep ByteStream
    decode = go len where
      go : (len : Nat) -> DecodePart (Vect len f.Rep) ByteStream
      go 0 = pure []
      go (S len) = [| f.decode :: go len |]

    encode : Encode Rep ByteStream
    encode = go len where
      go : (len : Nat) -> Encode (Vect len f.Rep) ByteStream
      go 0 [] = pure []
      go (S len) (x :: xs) = [| f.encode x <+> go len xs |]


  public export
  hrepeat : {len : Nat} -> Vect len Format -> Format
  hrepeat fs = MkFormat { Rep, decode, encode } where
    Rep : Type
    Rep = HVect (map (.Rep) fs)

    decode : DecodePart Rep ByteStream
    decode = go fs where
      go : {len : Nat} -> (fs : Vect len Format) -> DecodePart (HVect (map (.Rep) fs)) ByteStream
      go {len = Z} [] = pure []
      go {len = S _} (f :: fs) = [| f.decode :: go fs |]

    encode : Encode Rep ByteStream
    encode = go fs where
      go : {len : Nat} -> (fs : Vect len Format) -> Encode (HVect (map (.Rep) fs)) ByteStream
      go {len = Z} [] [] = pure []
      go {len = S _} (f :: fs) (x :: xs) = [| f.encode x <+> go fs xs |]


  public export
  pair : Format -> Format -> Format
  pair f1 f2 = MkFormat { Rep, decode, encode } where
    Rep : Type
    Rep = (f1.Rep, f2.Rep)

    decode : DecodePart Rep ByteStream
    decode =
      [| (,) f1.decode f2.decode |]

    encode : Encode Rep ByteStream
    encode (x, y) =
      [| f1.encode x <+> f2.encode y |]


  public export
  bind : (f : Format) -> (f.Rep -> Format) -> Format
  bind f1 f2 = MkFormat { Rep, decode, encode } where
    Rep : Type
    Rep = (x : f1.Rep ** (f2 x).Rep)

    decode : DecodePart Rep ByteStream
    decode = do
      x <- f1.decode
      y <- (f2 x).decode
      pure (x ** y)

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


namespace Format

  public export
  u8 : Format
  u8 = MkFormat
    { Rep = Nat
    , decode = map cast decodeU8
    , encode = encodeU8 . cast {to = Bits8}
    }


  public export
  u16Le : Format
  u16Le = MkFormat
    { Rep = Nat
    , decode = map cast (decodeU16 LE)
    , encode = encodeU16 LE . cast {to = Bits16}
    }


  public export
  u16Be : Format
  u16Be = MkFormat
    { Rep = Nat
    , decode = map cast (decodeU16 BE)
    , encode = encodeU16 BE . cast {to = Bits16}
    }


  public export
  u32Le : Format
  u32Le = MkFormat
    { Rep = Nat
    , decode = map cast (decodeU32 LE)
    , encode = encodeU32 LE . cast {to = Bits32}
    }


  public export
  u32Be : Format
  u32Be = MkFormat
    { Rep = Nat
    , decode = map cast (decodeU32 BE)
    , encode = encodeU32 BE . cast {to = Bits32}
    }


---------------------------------
-- INDEXED FORMAT DESCRIPTIONS --
---------------------------------


||| A format description refined with a fixed representation
public export
data FormatOf : (A : Type) -> Type where
  MkFormatOf : (f : Format) -> FormatOf f.Rep


namespace FormatOf

  decode : {0 A : Type} -> (f : FormatOf A) -> DecodePart (A) ByteStream
  decode  (MkFormatOf f) = Format.decode f


  encode : {0 A : Type} -> (f : FormatOf A) -> Encode A ByteStream
  encode  (MkFormatOf f) = Format.encode f


------------------------------------
-- FORMAT DESCRIPTION CONVERSIONS --
------------------------------------


namespace Format

  public export
  toFormatOf : (f : Format) -> FormatOf f.Rep
  toFormatOf f = MkFormatOf f


  ||| Convert a format description into an indexed format description with an
  ||| equality proof that the representation is the same as the index.
  public export
  toFormatOfEq : {0 A : Type} -> (Subset Format (\f => f.Rep = A)) -> FormatOf A
  toFormatOfEq (Element f prf) = rewrite sym prf in MkFormatOf f


namespace FormatOf

  public export
  toFormat : {0 A : Type} -> FormatOf A -> Format
  toFormat (MkFormatOf f) = f


  ||| Convert an indexed format description to a existential format description,
  ||| along with a proof that the representation is the same as the index.
  public export
  toFormatEq : {0 A : Type} -> FormatOf A -> (Subset Format (\f => f.Rep = A))
  toFormatEq (MkFormatOf f) = Element f Refl


public export
toFormatOfIso : Iso Format (Exists FormatOf)
toFormatOfIso = MkIso
  { to = \f => Evidence _ (toFormatOf f)
  , from = \(Evidence _ f) => toFormat f
  , toFrom = \(Evidence _ (MkFormatOf _)) => Refl
  , fromTo = \_ => Refl
  }


public export
toFormatOfEqIso : Iso (Exists (\a => (Subset Format (\f => Rep f = a)))) (Exists FormatOf)
toFormatOfEqIso = MkIso
  { to = \(Evidence _ f) => Evidence _ (toFormatOfEq f)
  , from = \(Evidence _ f) => Evidence _ (toFormatEq f)
  , toFrom = \(Evidence _ (MkFormatOf _)) => Refl
  , fromTo = \(Evidence _ (Element _ Refl)) => Refl
  }


---------------------------------
-- INDEXED FORMAT CONSTRUCTORS --
---------------------------------

-- Helpful constructors for building index format descriptions.
-- This also tests if we can actually meaningfully use the `FormatOf` type.

namespace FormatOf

  public export
  end : FormatOf Unit
  end = MkFormatOf end


  public export
  fail : FormatOf Void
  fail = MkFormatOf fail


  public export
  pure : {0 A : Type} -> (x : A) -> FormatOf (Sing x)
  pure x = MkFormatOf (pure x)


  public export
  ignore : {0 A : Type} -> (f : FormatOf A) -> (def : A) -> FormatOf Unit
  ignore f def with (toFormatEq f)
    ignore _ def | (Element f prf) = MkFormatOf (ignore f (rewrite prf in def))


  public export
  repeat : {0 A : Type} -> (len : Nat) -> FormatOf A -> FormatOf (Vect len A)
  repeat len f with (toFormatEq f)
    repeat len _ | (Element f prf) =
      toFormatOfEq (Element (repeat len f) (cong (Vect len) prf))


  public export
  pair : {0 A, B : Type} -> FormatOf A -> FormatOf B -> FormatOf (A, B)
  pair f1 f2 with (toFormatEq f1, toFormatEq f2)
    pair _ _ | (Element f1 prf1, Element f2 prf2) =
      toFormatOfEq (Element (pair f1 f2)
        (rewrite prf1 in rewrite prf2 in Refl))


  public export
  bind : {0 A : Type} -> {0 B : A -> Type} -> (f : FormatOf A) -> ((x : A) -> FormatOf (B x)) -> FormatOf (x : A ** B x)
  bind f1 f2 with (toFormatEq f1)
    bind _ f2 | (Element f1 prf) =
      ?todoFormatOf_bind


  public export
  (>>=) : {0 A : Type} -> {0 B : A -> Type} -> (f : FormatOf A) -> ((x : A) -> FormatOf (B x)) -> FormatOf (x : A ** B x)
  (>>=) = bind

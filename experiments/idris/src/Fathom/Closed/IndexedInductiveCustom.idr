||| A closed universe of format descriptions as an inductive type, where the
||| in-memory representation is tracked as an index on the type.

module Fathom.Closed.IndexedInductiveCustom


import Data.Colist
import Data.DPair
import Data.Vect

import Fathom.Base
import Fathom.Data.Iso
import Fathom.Data.Sing


---------------------------------
-- INDEXED FORMAT DESCRIPTIONS --
---------------------------------


||| A custom format description.
|||
||| We’d prefer to just import `Fathom.Open.Record`, but Idris’ imports are a
||| bit temperamental and result in ambiguities when importing modules that
||| contain types of the same name as those defined in the current module.
public export
record CustomFormat where
  constructor MkCustomFormat
  Rep : Type
  decode : Decode (Rep, ByteStream) ByteStream
  encode : Encode Rep ByteStream


||| Universe of format descriptions indexed by their machine representations
public export
data FormatOf : (A : Type) -> Type where
  End : FormatOf Unit
  Fail : FormatOf Void
  Pure : {0 A : Type} -> (x : A) -> FormatOf (Sing x)
  Skip : {0 A : Type} -> (f : FormatOf A) -> (def : A) -> FormatOf Unit
  Repeat : {0 A : Type} -> (len : Nat) -> FormatOf A -> FormatOf (Vect len A)
  Bind : {0 A : Type} -> {0 B : A -> Type} -> (f : FormatOf A) -> ((x : A) -> FormatOf (B x)) -> FormatOf (x : A ** B x)
  Custom :  (f : CustomFormat) -> FormatOf f.Rep


namespace FormatOf

  -- Support for do notation

  public export
  pure : {0 A : Type} -> (x : A) -> FormatOf (Sing x)
  pure = Pure

  public export
  (>>=) : {0 A : Type} -> {0 B : A -> Type} -> (f : FormatOf A) -> ((x : A) -> FormatOf (B x)) -> FormatOf (x : A ** B x)
  (>>=) = Bind


  ---------------------------
  -- ENCODER/DECODER PAIRS --
  ---------------------------


  export
  decode : {0 A : Type} -> (f : FormatOf A) -> DecodePart A ByteStream
  decode End =
    \case [] => Just ((), [])
          (_::_) => Nothing
  decode Fail = const Nothing
  decode (Pure x) = pure (MkSing x)
  decode (Skip f _) = map (const ()) (decode f)
  decode (Repeat 0 f) = pure []
  decode (Repeat (S len) f) = do
    x <- decode f
    xs <- decode (Repeat len f)
    pure (x :: xs)
  decode (Bind f1 f2) = do
    x <- decode f1
    y <- decode (f2 x)
    pure (x ** y)
  decode (Custom f) = f.decode


  export
  encode : {0 A : Type} -> (f : FormatOf A) -> Encode A ByteStream
  encode End () = Just []
  encode (Pure x) (MkSing _) = Just []
  encode (Skip f def) () = encode f def
  encode (Repeat Z f) [] = Just []
  encode (Repeat (S len) f) (x :: xs) =
    [| encode f x <+> encode (Repeat len f) xs |]
  encode (Bind f1 f2) (x ** y) =
    [| encode f1 x <+> encode (f2 x) y |]
  encode (Custom f) x = f.encode x


-------------------------
-- FORMAT DESCRIPTIONS --
-------------------------


||| A format description of an arbitrary representation
public export
record Format where
  constructor MkFormat
  ||| The in-memory representation of the format description
  0 Rep : Type
  ||| The underlying format description
  format : FormatOf Rep


namespace Format

  decode : (f : Format) -> Decode (Rep f, ByteStream) ByteStream
  decode f = FormatOf.decode f.format


  encode : (f : Format) -> Encode (Rep f) ByteStream
  encode f = FormatOf.encode f.format


------------------------------------
-- FORMAT DESCRIPTION CONVERSIONS --
------------------------------------


namespace Format

  public export
  toFormatOf : (f : Format) -> FormatOf f.Rep
  toFormatOf (MkFormat _ f) = f


  ||| Convert a format description into an indexed format description with an
  ||| equality proof that the representation is the same as the index.
  public export
  toFormatOfEq : {0 A : Type} -> (Subset Format (\f => f.Rep = A)) -> FormatOf A
  toFormatOfEq (Element f prf) = rewrite sym prf in f.format


namespace FormatOf

  public export
  toFormat : {0 A : Type} -> FormatOf A -> Format
  toFormat f = MkFormat A f


  ||| Convert an indexed format description to a existential format description,
  ||| along with a proof that the representation is the same as the index.
  public export
  toFormatEq : {0 A : Type} -> FormatOf A -> (Subset Format (\f => f.Rep = A))
  toFormatEq f = Element (MkFormat A f) Refl


public export
toFormatOfIso : Iso Format (Exists FormatOf)
toFormatOfIso = MkIso
  { to = \f => Evidence _ (toFormatOf f)
  , from = \(Evidence _ f) => toFormat f
  , toFrom = \(Evidence _ _) => Refl
  , fromTo = \(MkFormat _ _) => Refl
  }


public export
toFormatOfEqIso : Iso (Exists (\a => (Subset Format (\f => f.Rep = a)))) (Exists FormatOf)
toFormatOfEqIso = MkIso
  { to = \(Evidence _ f) => Evidence _ (toFormatOfEq f)
  , from = \(Evidence _ f) => Evidence _ (toFormatEq f)
  , toFrom = \(Evidence _ _) => Refl
  , fromTo = \(Evidence _ (Element (MkFormat _ _) Refl)) => Refl
  }


-------------------------
-- FORMAT CONSTRUCTORS --
-------------------------

-- Helpful constructors for building non-indexed format descriptions.
-- This also tests if we can actually meaningfully use the `Format` type.

namespace Format

  public export
  end : Format
  end = MkFormat () End


  public export
  fail : Format
  fail = MkFormat Void Fail


  public export
  pure : {0 A : Type} -> (x : A) -> Format
  pure x = MkFormat (Sing x) (Pure x)


  public export
  skip : (f : Format) -> (def : f.Rep) -> Format
  skip f def = MkFormat Unit (Skip (toFormatOf f) def)


  public export
  repeat : (len : Nat) -> Format -> Format
  repeat len f = MkFormat (Vect len f.Rep) (Repeat len (toFormatOf f))


  public export
  bind : (f : Format) -> (Rep f -> Format) -> Format
  bind f1 f2 =
      MkFormat (x : f1.Rep ** (f2 x).Rep)
        (Bind (toFormatOf f1) (\x => toFormatOf (f2 x)))


  public export
  (>>=) : (f : Format) -> (Rep f -> Format) -> Format
  (>>=) = bind


--------------------
-- CUSTOM FORMATS --
--------------------


public export
u8 : FormatOf Nat
u8 = Custom (MkCustomFormat
  { Rep = Nat
  , decode = map cast decodeU8
  , encode = encodeU8 . cast {to = Bits8}
  })


public export
u16Le : FormatOf Nat
u16Le = Custom (MkCustomFormat
  { Rep = Nat
  , decode = map cast (decodeU16 LE)
  , encode = encodeU16 LE . cast {to = Bits16}
  })


public export
u16Be : FormatOf Nat
u16Be = Custom (MkCustomFormat
  { Rep = Nat
  , decode = map cast (decodeU16 BE)
  , encode = encodeU16 BE . cast {to = Bits16}
  })

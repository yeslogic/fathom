||| Experimenting with an approach to extending inductive-recursive format
||| descriptions with custom formats.

module Fathom.Closed.InductiveRecursiveCustom


import Data.Bits
import Data.Colist
import Data.DPair
import Data.Vect

import Fathom.Base
import Fathom.Data.Iso
import Fathom.Data.Sing


public export
typeOf : {1 A : Type} -> (0 x : A) -> Type
typeOf _ = A


-------------------------
-- FORMAT DESCRIPTIONS --
-------------------------


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


mutual
  ||| Universe of format descriptions
  public export
  data Format : Type where
    End : Format
    Fail : Format
    Pure : {0 A : Type} -> A -> Format
    Ignore : (f : Format) -> (def : Rep f) -> Format
    Repeat : Nat -> Format -> Format
    Bind : (f : Format) -> (Rep f -> Format) -> Format
    Custom :  (f : CustomFormat) -> Format


  ||| The in-memory representation of format descriptions
  public export
  Rep : Format -> Type
  Rep End = Unit
  Rep Fail = Void
  Rep (Ignore _ _) = Unit
  Rep (Repeat len f) = Vect len (Rep f)
  Rep (Pure x) = Sing x
  Rep (Bind f1 f2) = (x : Rep f1 ** Rep (f2 x))
  Rep (Custom f) = f.Rep


namespace Format

  -- Support for do notation

  public export
  pure : {0 A : Type} -> A -> Format
  pure = Pure

  public export
  (>>=) : (f : Format) -> (Rep f -> Format) -> Format
  (>>=) = Bind


  ---------------------------
  -- ENCODER/DECODER PAIRS --
  ---------------------------


  export
  decode : (f : Format) -> DecodePart (Rep f) ByteStream
  decode End =
    \case [] => Just ((), [])
          (_::_) => Nothing
  decode Fail = const Nothing
  decode (Pure x) = pure (MkSing x)
  decode (Ignore f _) = ignore (decode f)
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
  encode : (f : Format) -> Encode (Rep f) ByteStream
  encode End () = Just []
  encode (Pure x) (MkSing _) = Just []
  encode (Ignore f def) () = encode f def
  encode (Repeat Z f) [] = Just []
  encode (Repeat (S len) f) (x :: xs) =
    [| encode f x <+> encode (Repeat len f) xs |]
  encode (Bind f1 f2) (x ** y) =
    [| encode f1 x <+> encode (f2 x) y |]
  encode (Custom f) x = f.encode x


  --------------------
  -- CUSTOM FORMATS --
  --------------------


  public export
  u8 : Format
  u8 = Custom (MkCustomFormat
    { Rep = Nat
    , decode = map cast decodeU8
    , encode = encodeU8 . cast {to = Bits8}
    })


  public export
  u16Le : Format
  u16Le = Custom (MkCustomFormat
    { Rep = Nat
    , decode = map cast (decodeU16 LE)
    , encode = encodeU16 LE . cast {to = Bits16}
    })


  public export
  u16Be : Format
  u16Be = Custom (MkCustomFormat
    { Rep = Nat
    , decode = map cast (decodeU16 BE)
    , encode = encodeU16 BE . cast {to = Bits16}
    })


---------------------------------
-- INDEXED FORMAT DESCRIPTIONS --
---------------------------------


||| A format description indexed with a fixed representation
public export
data FormatOf : (Rep : Type) -> Type where
  MkFormatOf : (f : Format) -> FormatOf (Rep f)


namespace FormatOf

  decode : {0 A : Type} -> (f : FormatOf A) -> Decode (A, ByteStream) ByteStream
  decode  (MkFormatOf f) = Format.decode f


  encode : {0 A : Type} -> (f : FormatOf A) -> Encode A ByteStream
  encode  (MkFormatOf f) = Format.encode f


------------------------------------
-- FORMAT DESCRIPTION CONVERSIONS --
------------------------------------


namespace Format

  public export
  toFormatOf : (f : Format) -> FormatOf (Rep f)
  toFormatOf f = MkFormatOf f


  ||| Convert a format description into an indexed format description with an
  ||| equality proof that the representation is the same as the index.
  public export
  toFormatOfEq : {0 A : Type} -> (Subset Format (\f => Rep f = A)) -> FormatOf A
  toFormatOfEq (Element f prf) = rewrite sym prf in MkFormatOf f


namespace FormatOf

  public export
  toFormat : {0 A : Type} -> FormatOf A -> Format
  toFormat (MkFormatOf f) = f


  ||| Convert an indexed format description to a existential format description,
  ||| along with a proof that the representation is the same as the index.
  public export
  toFormatEq : {0 A : Type} -> FormatOf A -> (Subset Format (\f => Rep f = A))
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
  end = MkFormatOf End


  public export
  fail : FormatOf Void
  fail = MkFormatOf Fail


  public export
  pure : {0 A : Type} -> (x : A) -> FormatOf (Sing x)
  pure x = MkFormatOf (Pure x)


  public export
  ignore : {0 A : Type} -> (f : FormatOf A) -> (def : A) -> FormatOf Unit
  ignore f def with (toFormatEq f)
    ignore _ def | (Element f prf) = MkFormatOf (Ignore f (rewrite prf in def))


  public export
  repeat : {0 A : Type} -> (len : Nat) -> FormatOf A -> FormatOf (Vect len A)
  repeat len f with (toFormatEq f)
    repeat len _ | (Element f prf) =
      toFormatOfEq (Element (Repeat len f) (cong (Vect len) prf))


  public export
  bind : {0 A : Type} -> {0 B : A -> Type} -> (f : FormatOf A) -> ((x : A) -> FormatOf (B x)) -> FormatOf (x : A ** B x)
  bind f1 f2 with (toFormatEq f1)
    bind _ f2 | (Element f1 prf) =
      ?todoFormatOf_bind


  public export
  (>>=) : {0 A : Type} -> {0 B : A -> Type} -> (f : FormatOf A) -> ((x : A) -> FormatOf (B x)) -> FormatOf (x : A ** B x)
  (>>=) = bind

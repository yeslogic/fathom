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
decode : {0 A : Type} -> (f : FormatOf A) -> Decode (A, ByteStream) (ByteStream)
decode End [] = Just ((), [])
decode End (_::_) = Nothing
decode Fail _ = Nothing
decode (Pure x) buffer =
  Just (MkSing x, buffer)
decode (Skip f _) buffer = do
  (x, buffer') <- decode f buffer
  Just ((), buffer')
decode (Repeat 0 f) buffer =
  Just ([], buffer)
decode (Repeat (S len) f) buffer = do
  (x, buffer') <- decode f buffer
  (xs, buffer'') <- decode (Repeat len f) buffer'
  Just (x :: xs, buffer'')
decode (Bind f1 f2) buffer = do
  (x, buffer') <- decode f1 buffer
  (y, buffer'') <- decode (f2 x) buffer'
  Just ((x ** y), buffer'')
decode (Custom f) buffer = f.decode buffer


export
encode : {0 A : Type} -> (f : FormatOf A) -> Encode A (ByteStream)
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


------------------------------------
-- FORMAT DESCRIPTION CONVERSIONS --
------------------------------------


public export
toFormatOf : (f : Format) -> FormatOf f.Rep
toFormatOf (MkFormat _ f) = f


public export
toFormat : {0 A : Type} -> FormatOf A -> Format
toFormat f = MkFormat A f


public export
toFormatOfIso : Iso Format (Exists FormatOf)
toFormatOfIso = MkIso
  { to = \f => Evidence _ (toFormatOf f)
  , from = \(Evidence _ f) => toFormat f
  , toFrom = \(Evidence _ _) => Refl
  , fromTo = \(MkFormat _ _) => Refl
  }


||| Convert a format description into an indexed format description with an
||| equality proof that the representation is the same as the index.
public export
toFormatOfEq : {0 A : Type} -> (Subset Format (\f => f.Rep = A)) -> FormatOf A
toFormatOfEq (Element f prf) = rewrite sym prf in f.format


||| Convert an indexed format description to a existential format description,
||| along with a proof that the representation is the same as the index.
public export
toFormatEq : {0 A : Type} -> FormatOf A -> (Subset Format (\f => f.Rep = A))
toFormatEq f = Element (MkFormat A f) Refl


public export
toFormatOfEqIso : Iso (Exists (\a => (Subset Format (\f => f.Rep = a)))) (Exists FormatOf)
toFormatOfEqIso = MkIso
  { to = \(Evidence _ f) => Evidence _ (toFormatOfEq f)
  , from = \(Evidence _ f) => Evidence _ (toFormatEq f)
  , toFrom = \(Evidence _ _) => Refl
  , fromTo = \(Evidence _ (Element (MkFormat _ _) Refl)) => Refl
  }


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


-----------------
-- EXPERIMENTS --
-----------------

-- Reproduction of difficulties in OpenType format


Flag : Type
Flag =
  (  id : Nat
  ** repeat :
    case id of
      0 => Nat
      S n => Sing {A = Nat} 0
  ** Sing ()
  )

(.repeat) : Flag -> Nat
(.repeat) (0 ** repeat ** _) = repeat
(.repeat) (S _ ** repeat ** _) = val repeat


-- def flag = {
--     flag <- u8,
--     repeat <- match ((u8_and flag 8) != (0 : U8)) {
--       true => u8,
--       false => succeed U8 0,
--     },
-- };
flag : FormatOf Flag
flag = do
  flag <- u8
  repeat <- case flag of
    0 => u8
    S _ => Pure {A = Nat} 0
  Pure ()


SimpleGlyph : Type
SimpleGlyph =
  (  flag : Flag
  ** Sing (flag.repeat + 1)
  )


-- def simple_glyph = fun (number_of_contours : U16) => {
--     ...
--     let flag_repeat = fun (f : Repr flag) => f.repeat + (1 : U8),
--     ...
-- };
simple_glyph : FormatOf SimpleGlyph
simple_glyph = do
  flag <- flag
  Pure (flag.repeat + 1)

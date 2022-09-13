||| A closed universe of format descriptions as an inductive type, where the
||| in-memory representation is tracked as an index on the type.

module Fathom.Format.IndexedInductive


import Data.Colist
import Data.DPair
import Data.HVect
import Data.Vect

import Fathom.Base
import Fathom.Data.Iso
import Fathom.Data.Sing


---------------------------------
-- INDEXED FORMAT DESCRIPTIONS --
---------------------------------


||| Universe of format descriptions indexed by their machine representations
public export
data FormatOf : Type -> Type where
  End : FormatOf Unit
  Fail : FormatOf Void
  Pure : {0 A : Type} -> (x : A) -> FormatOf (Sing x)
  Ignore : {0 A : Type} -> (f : FormatOf A) -> (def : A) -> FormatOf Unit
  Choice : {0 A, B : Type} -> FormatOf A -> FormatOf B -> FormatOf (Either A B)
  Repeat : {0 A : Type} -> (len : Nat) -> FormatOf A -> FormatOf (Vect len A)
  Tuple : {reps : Vect len Type} -> HVect (map FormatOf reps) -> FormatOf (HVect reps)
  Pair : {0 A, B : Type} -> FormatOf A -> FormatOf B -> FormatOf (A, B)
  Bind : {0 A : Type} -> {0 B : A -> Type} -> (f : FormatOf A) -> ((x : A) -> FormatOf (B x)) -> FormatOf (x : A ** B x)


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
  decode : {0 A, S : Type} -> (f : FormatOf A) -> Decode (A, Colist S) (Colist S)
  decode End =
    \case [] => Just ((), [])
          (_::_) => Nothing
  decode Fail = const Nothing
  decode (Pure x) = pure (MkSing x)
  decode (Ignore f _) = ignore (decode f)
  decode (Choice f1 f2) =
    [| Left (decode f1) |] <|> [| Right (decode f2) |]
  decode (Repeat 0 f) = pure []
  decode (Repeat (S len) f) =
    [| decode f :: decode (Repeat len f) |]
  decode (Tuple {reps = []} []) = pure []
  decode (Tuple {reps = _::_} (f :: fs)) = DecodePart.do
    x <- decode f
    xs <- decode (Tuple fs)
    pure (x :: xs)
    -- FIXME: Ambiguous elaboration for some reason??
    -- [| decode f :: decode (Tuple fs) |]
  decode (Pair f1 f2) =
    [| (,) (decode f1) (decode f2) |]
  decode (Bind f1 f2) = do
    x <- decode f1
    y <- decode (f2 x)
    pure (x ** y)


  export
  encode : {0 A, S : Type} -> (f : FormatOf A) -> Encode A (Colist S)
  encode End () = pure []
  encode (Pure x) (MkSing _) = pure []
  encode (Ignore f def) () = encode f def
  encode (Choice f1 f2) (Left x) = encode f1 x
  encode (Choice f1 f2) (Right y) = encode f2 y
  encode (Repeat Z f) [] = pure []
  encode (Repeat (S len) f) (x :: xs) =
    [| encode f x <+> encode (Repeat len f) xs |]
  encode (Tuple {reps = []} []) [] = pure []
  encode (Tuple {reps = _::_} (f :: fs)) (x :: xs) =
    [| encode f x <+> encode (Tuple fs) xs |]
  encode (Pair f1 f2) (x, y) =
    [| encode f1 x <+> encode f2 y |]
  encode (Bind f1 f2) (x ** y) =
    [| encode f1 x <+> encode (f2 x) y |]


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

  decode : (f : Format) -> Decode (Rep f, Colist a) (Colist a)
  decode f = FormatOf.decode f.format


  encode : (f : Format) -> Encode (Rep f) (Colist a)
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
  skip f def = MkFormat Unit (Ignore (toFormatOf f) def)


  public export
  repeat : (len : Nat) -> Format -> Format
  repeat len f = MkFormat (Vect len f.Rep) (Repeat len (toFormatOf f))


  public export
  pair : Format -> Format -> Format
  pair f1 f2 = MkFormat (f1.Rep, f2.Rep) (Pair (toFormatOf f1) (toFormatOf f2))


  public export
  bind : (f : Format) -> (Rep f -> Format) -> Format
  bind f1 f2 =
      MkFormat (x : f1.Rep ** (f2 x).Rep)
        (Bind (toFormatOf f1) (\x => toFormatOf (f2 x)))


  public export
  (>>=) : (f : Format) -> (Rep f -> Format) -> Format
  (>>=) = bind

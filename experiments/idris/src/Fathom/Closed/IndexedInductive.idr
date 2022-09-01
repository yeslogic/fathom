||| A closed universe of format descriptions as an inductive type, where the
||| in-memory representation is tracked as an index on the type.

module Fathom.Closed.IndexedInductive


import Data.Colist
import Data.DPair
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
  Skip : {0 A : Type} -> (f : FormatOf A) -> (def : A) -> FormatOf Unit
  Repeat : {0 A : Type} -> (len : Nat) -> FormatOf A -> FormatOf (Vect len A)
  Bind : {0 A : Type} -> {0 B : A -> Type} -> (f : FormatOf A) -> ((x : A) -> FormatOf (B x)) -> FormatOf (x : A ** B x)


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

-- export
-- decode : {0 A, S : Type} -> (f : FormatOf A) -> Decode (A, Colist S) (Colist S)
-- decode End
--   = \buffer => case buffer of
--       [] => Just ((), [])
--       _::_ => Nothing
-- decode Fail
--   = const Nothing
-- decode (Pure x)
--   = pure (MkSing x)
-- decode (Skip f _)
--   = do  _ <- decode f
--         pure ()
-- decode (Repeat 0 f) = pure []
-- decode (Repeat (S len) f)
--   = do  x <- decode f
--         xs <- decode (Repeat len f)
--         pure (x :: xs)
-- decode (Bind f1 f2)
--   = do  x <- decode f1
--         y <- decode (f2 x)
--         pure (x ** y)


export
encode : {0 A, S : Type} -> (f : FormatOf A) -> Encode A (Colist S)
encode End () = Just []
encode (Pure x) (MkSing _) = Just []
encode (Skip f def) () = encode f def
encode (Repeat Z f) [] = Just []
encode (Repeat (S len) f) (x :: xs) =
  [| encode f x <+> encode (Repeat len f) xs |]
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
toFormatOfEq : {0 A : Type} -> (f : Format ** f.Rep = A) -> FormatOf A
toFormatOfEq (f ** prf) = rewrite sym prf in f.format


||| Convert an indexed format description to a existential format description,
||| along with a proof that the representation is the same as the index.
public export
toFormatEq : {0 A : Type} -> FormatOf A -> (f : Format ** f.Rep = A)
toFormatEq f = (MkFormat A f ** Refl)


public export
toFormatOfEqIso : Iso (Exists (\a => (f : Format ** f.Rep = a))) (Exists FormatOf)
toFormatOfEqIso = MkIso
  { to = \(Evidence _f) => Evidence _ (toFormatOfEq f)
  , from = \(Evidence _ f) => Evidence _ (toFormatEq f)
  , toFrom = \(Evidence _ _) => Refl
  , fromTo = \(Evidence _ ((MkFormat _ _) ** Refl)) => Refl
  }


-----------------
-- EXPERIMENTS --
-----------------


either : (cond : Bool) -> FormatOf a -> FormatOf b -> FormatOf (if cond then a else b)
either True f1 _ = f1
either False _ f2 = f2

orPure : (cond : Bool) -> FormatOf a -> (def : a) -> FormatOf (if cond then a else Sing def)
orPure True f _ = f
orPure False _ def = Pure def

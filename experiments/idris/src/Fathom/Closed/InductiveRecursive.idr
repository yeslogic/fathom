||| A closed universe of format descriptions, using induction recursion between
||| the descriptions and their in-memory representation. This closely matches
||| the current implementation of format descriptions in Fathom.
|||
||| [Induction recursion](https://en.wikipedia.org/wiki/Induction-recursion) is
||| where an inductive datatype is defined simultaneously with a function that
||| operates on that type (see the @Format and @Rep definitions below).
|||
||| The universe is ‘closed’ in the sense tha new format descriptions cannot be
||| added to the type theory, although they can be composed out of other formats)
|||
||| This is similar to the approach used when defining type theories with
||| Tarski-style universes. In-fact inductive-recursive datatypes as a language
||| feature were apparently originally motivated by this use case (see: [“A
||| General Formulation of Simultaneous Inductive-Recursive Definitions in Type
||| Theory”](https://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.6.4575) by
||| Dybjer).
|||
||| Inspiration for this approach is taken from [“The Power of Pi”](https://cs.ru.nl/~wouters/Publications/ThePowerOfPi.pdf)
||| by Oury and Swierstra.

module Fathom.Closed.InductiveRecursive


import Data.Colist
import Data.Vect

import Fathom.Base
import Fathom.Data.Sing

-- import Fathom.Open.Record


-------------------------
-- FORMAT DESCRIPTIONS --
-------------------------


mutual
  ||| Universe of format descriptions
  public export
  data Format : Type where
    End : Format
    Fail : Format
    Pure : {0 A : Type} -> A -> Format
    Skip : (f : Format) -> (def : Rep f) -> Format
    Repeat : Nat -> Format -> Format
    Bind : (f : Format) -> (Rep f -> Format) -> Format

    -- Questionable format descriptions
    -- OrPure : (cond : Bool) -> (f : Format) -> (def : Rep f) -> Format
    -- OfSing : (f : Format) -> Sing (Rep f) -> Format
    -- OfEq : (f : Format) -> (r : Type) -> {auto 0 prf : Rep f = r} -> Format

    -- Broken stuff
    -- Let : (f : Format) -> (Rep f -> Format) -> Format
    -- Custom : (f : Record.Format) -> Format


  ||| The in-memory representation of format descriptions
  public export
  Rep : Format -> Type
  Rep End = Unit
  Rep Fail = Void
  Rep (Skip _ _) = Unit
  Rep (Repeat len f) = Vect len (Rep f)
  Rep (Pure x) = Sing x
  Rep (Bind f1 f2) = (x : Rep f1 ** Rep (f2 x))

  -- Questionable format descriptions
  -- Rep (OrPure _ f _) = Rep f
  -- Rep (OfSing f r) = value r
  -- Rep (OfEq f r) = r

  -- Broken stuff
  -- Rep (Let f1 f2) = Rep (f2 ?halp)
  -- Rep (Custom f) = f.Rep


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
decode : (f : Format) -> Decode (Rep f, Colist a) (Colist a)
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


export
encode : (f : Format) -> Encode (Rep f) (Colist a)
encode End () = Just []
encode (Pure x) (MkSing _) = Just []
encode (Skip f def) () = encode f def
encode (Repeat Z f) [] = Just []
encode (Repeat (S len) f) (x :: xs) = do
  [| encode f x <+> encode (Repeat len f) xs |]
encode (Bind f1 f2) (x ** y) = do
  [| encode f1 x <+> encode (f2 x) y |]


---------------------------------
-- INDEXED FORMAT DESCRIPTIONS --
---------------------------------


||| A format description refined with a fixed representation
public export
data FormatOf : (0 A : Type) -> Type where
  MkFormatOf : (f : Format) -> FormatOf (Rep f)


------------------------------------
-- FORMAT DESCRIPTION CONVERSIONS --
------------------------------------


public export
toFormatOf : (f : Format) -> FormatOf (Rep f)
toFormatOf f = MkFormatOf f


public export
toFormat : {0 A : Type} -> FormatOf A -> Format
toFormat (MkFormatOf f) = f


public export
toFormatOfEq : {0 A : Type} -> (f : Format ** Rep f = A) -> FormatOf A
toFormatOfEq (f ** prf) = rewrite sym prf in MkFormatOf f


public export
toFormatEq : {0 A : Type} -> FormatOf A -> (f : Format ** Rep f = A)
toFormatEq (MkFormatOf f) = (f ** Refl)


export
either : (cond : Bool) -> (f1 : Format) -> (f2 : Format) -> FormatOf (if cond then Rep f1 else Rep f2)
either True f1 _ = MkFormatOf f1
either False _ f2 = MkFormatOf f2


public export
orPure : (cond : Bool) -> FormatOf a -> (def : a) -> FormatOf (if cond then a else Sing def)
orPure True f _ = f
orPure False _ def = MkFormatOf (Pure def)


public export
orPure' : (cond : Bool) -> FormatOf a -> (def : a) -> FormatOf (if cond then a else Sing def)
orPure' True f _ = f
orPure' False _ def = MkFormatOf (Pure def)

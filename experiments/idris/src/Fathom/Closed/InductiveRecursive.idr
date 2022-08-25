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


  ||| In-memory representation of format descriptions
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


---------------------------
-- ENCODER/DECODER PAIRS --
---------------------------


export
decode : (f : Format) -> Decode (Rep f) (Colist a)
decode End [] = Just ((), [])
decode End (_::_) = Nothing
decode Fail _ = Nothing
decode (Pure x) buffer =
  Just (sing x, buffer)
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

-- Questionable format descriptions
-- decode (OrPure True f _) buffer = decode f buffer
-- decode (OrPure False _ def) buffer = Just (def, buffer)
-- decode (OfSing f (MkSing r {prf})) buffer = do
--   (x, buffer') <- decode f buffer
--   Just (rewrite sym prf in x, buffer')
-- decode (OfEq f _ {prf}) buffer = do
--   (x, buffer') <- decode f buffer
--   Just (rewrite sym prf in x, buffer')

-- Broken stuff


export
encode : (f : Format) -> Encode (Rep f) (Colist a)
encode End () _ = Just []
encode (Pure x) (MkSing _) buffer = Just buffer
encode (Skip f def) () buffer = do
  encode f def buffer
encode (Repeat Z f) [] buffer = Just buffer
encode (Repeat (S len) f) (x :: xs) buffer = do
  buffer' <- encode (Repeat len f) xs buffer
  encode f x buffer'
encode (Bind f1 f2) (x ** y) buffer = do
  buffer' <- encode (f2 x) y buffer
  encode f1 x buffer'
-- Questionable format descriptions
-- encode (OrPure True f _) x buffer = encode f x buffer
-- encode (OrPure False _ def) x buffer = Just buffer
-- encode (OfSing f r) x buffer = do
--   buffer' <- encode f ?todo_x buffer
--   ?todo_encode
-- encode (OfEq f _ {prf}) x buffer = do
--   encode f (rewrite prf in x) buffer


-----------------
-- EXPERIMENTS --
-----------------


||| A format description refined with a fixed representation
public export
FormatOf : (0 Rep : Type) -> Type
FormatOf rep = Refine Format (\f => Rep f = rep)


toFormatOf : (f : Format) -> FormatOf (Rep f)
toFormatOf f = refine f


export
either : (cond : Bool) -> (f1 : Format) -> (f2 : Format) -> FormatOf (if cond then Rep f1 else Rep f2)
either True f1 _ = refine f1
either False _ f2 = refine f2


export
orPure : (cond : Bool) -> FormatOf a -> (def : a) -> FormatOf (if cond then a else Sing def)
orPure True f _ = f
orPure False _ def = refine (Pure def)


export
orPure' : (cond : Bool) -> FormatOf a -> (def : a) -> FormatOf (if cond then a else Sing def)
orPure' True f _ = f
orPure' False _ def = refine (Pure def)


foo : (cond : Bool) -> (f : Format) -> Rep f -> Format
foo cond f def = case orPure cond (toFormatOf f) def of
  MkRefine f' {prf} =>
    Bind f' (\x => case cond of
      True => ?todo1
      False => ?todo2)

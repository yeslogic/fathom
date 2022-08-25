||| A closed universe of format descriptions as an inductive type, where the
||| in-memory representation is tracked as an index on the type.

module Fathom.Closed.IndexedInductive


import Data.Colist
import Data.Vect

import Fathom.Base
import Fathom.Data.Sing


-------------------------
-- FORMAT DESCRIPTIONS --
-------------------------


||| Universe of format descriptions indexed by their machine representations
public export
data FormatOf : (0 Rep : Type) -> Type where
  End : FormatOf Unit
  Fail : FormatOf Void
  Pure : {0 A : Type} -> (x : A) -> FormatOf (Sing x)
  Skip : {0 A : Type} -> (f : FormatOf A) -> (def : A) -> FormatOf Unit
  Repeat : {0 A : Type} -> (len : Nat) -> FormatOf A -> FormatOf (Vect len A)
  Bind : {0 A : Type} -> {0 B : A -> Type} -> (f : FormatOf A) -> ((x : A) -> FormatOf (B x)) -> FormatOf (x : A ** B x)



---------------------------
-- ENCODER/DECODER PAIRS --
---------------------------

export
decode : {0 Rep : Type} -> (f : FormatOf Rep) -> Decode Rep (Colist a)
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
encode : {0 Rep : Type} -> (f : FormatOf Rep) -> Encode Rep (Colist a)
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


-----------------
-- EXPERIMENTS --
-----------------


either : (cond : Bool) -> FormatOf a -> FormatOf b -> FormatOf (if cond then a else b)
either True f1 _ = f1
either False _ f2 = f2

orPure : (cond : Bool) -> FormatOf a -> (def : a) -> FormatOf (if cond then a else Sing def)
orPure True f _ = f
orPure False _ def = Pure def

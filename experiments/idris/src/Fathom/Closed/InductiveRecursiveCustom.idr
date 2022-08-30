||| Experimenting with an approach to extending inductive-recursive format
||| descriptions with custom formats.

module Fathom.Closed.InductiveRecursiveCustom


import Data.Colist
import Data.Vect

import Fathom.Base
import Fathom.Data.Sing
import Fathom.Data.Refine


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
  decode : Decode Rep ByteStream
  encode : Encode Rep ByteStream


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
    Custom :  (f : CustomFormat) -> Format


  ||| In-memory representation of format descriptions
  public export
  Rep : Format -> Type
  Rep End = Unit
  Rep Fail = Void
  Rep (Skip _ _) = Unit
  Rep (Repeat len f) = Vect len (Rep f)
  Rep (Pure x) = Sing x
  Rep (Bind f1 f2) = (x : Rep f1 ** Rep (f2 x))
  Rep (Custom f) = f.Rep


---------------------------
-- ENCODER/DECODER PAIRS --
---------------------------


export
decode : (f : Format) -> Decode (Rep f) ByteStream
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
encode : (f : Format) -> Encode (Rep f) ByteStream
encode End () = Just []
encode (Pure x) (MkSing _) = Just []
encode (Skip f def) () = encode f def
encode (Repeat Z f) [] = Just []
encode (Repeat (S len) f) (x :: xs) =
  [| encode f x <+> encode (Repeat len f) xs |]
encode (Bind f1 f2) (x ** y) =
  [| encode f1 x <+> encode (f2 x) y |]
encode (Custom f) x = f.encode x


--------------
-- NOTATION --
--------------

-- Support for do notation

public export
pure : {0 A : Type} -> A -> Format
pure = Pure

public export
(>>=) : (f : Format) -> (Rep f -> Format) -> Format
(>>=) = Bind


--------------------
-- CUSTOM FORMATS --
--------------------


public export
u8 : Format
u8 = Custom (MkCustomFormat
  { Rep = Bits8
  , decode = \buffer =>
    case buffer of
      [] => Nothing
      x :: buffer => Just (x, buffer)
  , encode = \x => Just [x]
  })


-----------------
-- EXPERIMENTS --
-----------------


||| A format description refined with a fixed representation
public export
FormatOf : (0 Rep : Type) -> Type
FormatOf rep = Refine Format (\f => Rep f = rep)


toFormatOf : (f : Format) -> FormatOf (Rep f)
toFormatOf f = MkRefine f


export
either : (cond : Bool) -> (f1 : Format) -> (f2 : Format) -> FormatOf (if cond then Rep f1 else Rep f2)
either True f1 _ = MkRefine f1
either False _ f2 = MkRefine f2


export
orPure : (cond : Bool) -> FormatOf a -> (def : a) -> FormatOf (if cond then a else Sing def)
orPure True f _ = f
orPure False _ def = MkRefine (Pure def)


export
orPure' : (cond : Bool) -> FormatOf a -> (def : a) -> FormatOf (if cond then a else Sing def)
orPure' True f _ = f
orPure' False _ def = MkRefine (Pure def)


foo : (cond : Bool) -> (f : Format) -> Rep f -> Format
foo cond f def = case orPure cond (toFormatOf f) def of
  MkRefine f' {prf} =>
    Bind f' (\x => case cond of
      True => ?todo1
      False => ?todo2)


-- Reproduction of difficulties in OpenType format

-- def flag = {
--     flag <- u8,
--     repeat <- match ((u8_and flag 8) != (0 : U8)) {
--       true => u8,
--       false => succeed U8 0,
--     },
-- };
flag : Format
flag = do
  flag <- u8
  if flag == 0 then u8 else
    Pure {A = Bits8} 0

-- def simple_glyph = fun (number_of_contours : U16) => {
--     ...
--     let flag_repeat = fun (f : Repr flag) => f.repeat + (1 : U8),
--     ...
-- };
simple_glyph : Format
simple_glyph = do
  (flag ** repeat) <- flag
  let
    repeat' : Bits8
    repeat' = case flag of
      0 => repeat
      x => ?todo4

    -- repeat' : Bits8
    -- repeat' with (flag)
    --   repeat' | 0 = ?todo3
    --   repeat' | x = ?todo4

    -- repeat' : Bits8
    -- repeat' with (MkSingEq flag)
    --   repeat' | MkSingEq 0 {prf} = ?help
    --   repeat' | MkSingEq x {prf} = ?todo4

    -- repeat' : Bits8
    -- repeat' = case MkSingEq flag of
    --   MkSingEq 0 {prf} => ?todo3
    --   MkSingEq x {prf} => ?todo4
  Pure (repeat' + 1)

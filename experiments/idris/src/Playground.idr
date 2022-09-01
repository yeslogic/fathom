module Playground


import Data.Colist
import Data.Vect

import Fathom.Base
import Fathom.Data.Sing
import Fathom.Closed.InductiveRecursive as IndRec
import Fathom.Closed.IndexedInductive as Indexed
import Fathom.Open.Record as Record


-- Experiment with converting between the different styles of format universes


||| Convert an inductive-recursive format universe into a record format
public export
format : IndRec.Format -> Record.Format
format f = Record.MkFormat
  { Rep = IndRec.Rep f
  , decode = IndRec.decode f
  , encode = IndRec.encode f
  }


||| Convert an indexed-inductive format universe into a record format
public export
formatOf : {Rep : Type} -> Indexed.FormatOf Rep -> Record.Format
formatOf f = Record.MkFormat
  { Rep = Rep
  , decode = Indexed.decode f
  , encode = Indexed.encode f
  }


-- public export
-- format' : IndRec.Format -> Record.Format
-- format' f = MkFormat { Rep, decode, encode } where
--   Rep : Type
--   Rep = IndRec.Rep f

--   decode : Decode (IndRec.Rep f) BitStream
--   decode = case f of
--     End => end.decode
--     Fail => fail.decode
--     Pure x => (pure x).decode
--     Skip f def => (skip (format' f) def).decode
--     Repeat len f => (repeat len (format' f)).decode
--     Bind f1 f2 => (bind (format' f1) (\x => format' (f2 x))).decode
--     OfSing f r => (format' f).decode
--     OfEq f r => (format' f).decode

--   encode : Encode Rep BitStream
--   encode = case f of
--     End => end.encode
--     Fail => fail.encode
--     Pure x => (pure x).encode
--     Skip f def => (skip (format' f) def).encode
--     Repeat len f => (repeat len (format' f)).encode
--     Bind f1 f2 => (bind (format' f1) (\x => format' (f2 x))).encode
--     OfSing f r => (format' f).encode
--     OfEq f r => (format' f).encode


||| Convert an inductive-recursive format description to an indexed format
indRecToIndexed : (f : IndRec.Format) -> Indexed.FormatOf (Rep f)
indRecToIndexed End = Indexed.End
indRecToIndexed Fail = Indexed.Fail
indRecToIndexed (Pure x) = Indexed.Pure x
indRecToIndexed (Skip f def) = Indexed.Skip (indRecToIndexed f) def
indRecToIndexed (Repeat len f) = Indexed.Repeat len (indRecToIndexed f)
indRecToIndexed (Bind f g) = Indexed.Bind (indRecToIndexed f) (\x => indRecToIndexed (g x))


mutual

  ||| Convert an indexed format description to an inductive-recursive format
  indexedToIndRecFormat : (f : Indexed.Format) -> (f' : IndRec.Format ** Rep f = Rep f')
  indexedToIndRecFormat (MkFormat () End) = (End ** Refl)
  indexedToIndRecFormat (MkFormat Void Fail) = (Fail ** Refl)
  indexedToIndRecFormat (MkFormat (Sing x) (Pure x)) = (Pure x ** Refl)
  indexedToIndRecFormat (MkFormat () (Skip f def)) with (indexedToIndRecFormatOf f)
    _ | MkFormatOf f' = (Skip f' def ** Refl)
  indexedToIndRecFormat (MkFormat (Vect len _) (Repeat len f)) with (indexedToIndRecFormatOf f)
    _ | MkFormatOf f' = (Repeat len f' ** Refl)
  indexedToIndRecFormat (MkFormat (x : _ ** _) (Bind f1 f2)) with (indexedToIndRecFormatOf f1)
    _ | MkFormatOf f1' =
      (Bind f1' (\x => ?indexedToIndRecFormatBind_f2) ** ?todoBindPrf)


  ||| Convert an indexed format description to an inductive-recursive format
  indexedToIndRecFormatOf : {0 A : Type} -> (f : Indexed.FormatOf A) -> IndRec.FormatOf A
  indexedToIndRecFormatOf End = MkFormatOf End
  indexedToIndRecFormatOf Fail = MkFormatOf Fail
  indexedToIndRecFormatOf (Pure x) = MkFormatOf (Pure x)
  indexedToIndRecFormatOf (Skip f def) with (indexedToIndRecFormatOf f)
    _ | MkFormatOf f' = MkFormatOf (Skip f' def)
  indexedToIndRecFormatOf (Repeat len f) with (indexedToIndRecFormatOf f)
    _ | MkFormatOf f' = MkFormatOf (Repeat len f')
  indexedToIndRecFormatOf (Bind f1 f2) with (indexedToIndRecFormatOf f1)
    _ | MkFormatOf f1' =
      ?indexedToIndRecFormatOfBind


-- Reproduction of difficulties in OpenType format, drawing parallels to
-- Tarski-style universes.

repeatWithId : Nat -> Type
repeatWithId 0 = Nat
repeatWithId (S _) = Sing {A = Nat} 0

record Flag where
  constructor MkFlag
  id : Nat
  repeat : case id of
    0 => Nat
    (S n) => Sing {A = Nat} 0

record SimpleGlyph where
  constructor MkSimpleGlyph
  flag : Flag
  flag_repeat : Sing {A = Nat} (case flag of
    MkFlag 0 repeat => repeat
    MkFlag (S n) repeat => val repeat)

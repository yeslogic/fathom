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
-- indRecToIndexed (OfSing f (MkSing _ {prf})) = rewrite sym prf in indRecToIndexed f
-- indRecToIndexed (OfEq f _ {prf}) = rewrite sym prf in indRecToIndexed f


-- ||| Convert an indexed format description to an inductive-recursive format
-- indexedToIndRec : {0 Rep : Type} -> (f : Indexed.FormatOf Rep) -> IndRec.FormatOf Rep
-- indexedToIndRec End = MkRefine { value = IndRec.End, prf = Refl }
-- indexedToIndRec Fail = MkRefine { value = IndRec.Fail, prf = Refl }
-- indexedToIndRec (Pure x) = MkRefine { value = IndRec.Pure x, prf = Refl }
-- indexedToIndRec (Skip {a} f def) =
--   let
--     MkRefine f' prf = indexedToIndRec f
--     symPrf = sym prf
--     def' = rewrite prf in def
--   in
--     MkRefine { value = IndRec.Skip f' ?todoDef, prf = ?todoSkip }
-- indexedToIndRec (Repeat len f) = MkRefine { value = IndRec.Repeat _ _, prf = ?todoRepeat }
-- indexedToIndRec (Bind f g) = MkRefine { value = IndRec.Bind _ _, prf = ?todoBind }

||| Convert an indexed format description to an inductive-recursive format
indexedToIndRec : {0 Rep : Type} -> (f : Indexed.FormatOf Rep) -> IndRec.Format
indexedToIndRec End = IndRec.End
indexedToIndRec Fail = IndRec.Fail
indexedToIndRec (Pure x) = IndRec.Pure x
indexedToIndRec (Skip f def) =
  IndRec.Skip (indexedToIndRec f) ?todo_def
--                                ^^^^^^^^^
-- Error: While processing right hand side of indexedToIndRec. Can't solve constraint between: a and Rep (indexedToIndRec f).
--
--    def : a
--    f : FormatOf a
--  0 Rep : Type
-- ------------------------------
-- todo_def : Rep (indexedToIndRec f)
--
indexedToIndRec (Repeat len f) = IndRec.Repeat len (indexedToIndRec f)
indexedToIndRec (Bind f1 f2) = IndRec.Bind (indexedToIndRec f1) (\x => indexedToIndRec ?todo_f2)
--                                                                                     ^^^^^^^^
-- Error: While processing right hand side of indexedToIndRec. Can't solve constraint
-- between: Rep (indexedToIndRec f1) and a (implicitly bound at Fathom.Test:86:1--86:95).
--
--    f2 : (x : a) -> FormatOf (b x)
--    f1 : FormatOf a
--  0 Rep : Type
--    x : Rep (indexedToIndRec f1)
-- ------------------------------
-- todo_f2 : FormatOf ?Rep

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


||| Convert an indexed format description to an inductive-recursive format
indexedToIndRec : {0 A : Type} -> (f : Indexed.FormatOf A) -> IndRec.FormatOf A
indexedToIndRec End = MkFormatOf IndRec.End
indexedToIndRec Fail = MkFormatOf IndRec.Fail
indexedToIndRec (Pure x) = MkFormatOf (IndRec.Pure x)
indexedToIndRec (Skip f def) with (indexedToIndRec f)
  indexedToIndRec (Skip _ def) | MkFormatOf f = MkFormatOf (IndRec.Skip f def)
indexedToIndRec (Repeat len f) with (indexedToIndRec f)
  indexedToIndRec (Repeat len _) | MkFormatOf f = MkFormatOf (IndRec.Repeat len f)
indexedToIndRec (Bind f1 f2) with (indexedToIndRec f1)
  indexedToIndRec (Bind _ f2) | MkFormatOf f1 =
    ?todo_indexedToIndRec

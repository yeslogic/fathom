module Playground


import Data.Colist
import Data.Vect
import Data.HVect

import Fathom.Base
import Fathom.Data.Sing
import Fathom.Format.InductiveRecursive as IndRec
import Fathom.Format.IndexedInductive as Indexed
import Fathom.Format.Record as Record


-- Experiment with converting between the different styles of format universes


||| Convert an inductive-recursive format universe into a record format
public export
format : IndRec.Format -> Record.Format
format f = Record.MkFormat
  { Rep = f.Rep
  , decode = decode f
  , encode = encode f
  }


||| Convert an indexed-inductive format universe into a record format
public export
formatOf : {Rep : Type} -> Indexed.FormatOf Rep -> Record.Format
formatOf f = Record.MkFormat
  { Rep = Rep
  , decode = decode f
  , encode = encode f
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
--     Ignore f def => (skip (format' f) def).decode
--     Repeat len f => (repeat len (format' f)).decode
--     Bind f1 f2 => (bind (format' f1) (\x => format' (f2 x))).decode
--     OfSing f r => (format' f).decode
--     OfEq f r => (format' f).decode

--   encode : Encode Rep BitStream
--   encode = case f of
--     End => end.encode
--     Fail => fail.encode
--     Pure x => (pure x).encode
--     Ignore f def => (skip (format' f) def).encode
--     Repeat len f => (repeat len (format' f)).encode
--     Bind f1 f2 => (bind (format' f1) (\x => format' (f2 x))).encode
--     OfSing f r => (format' f).encode
--     OfEq f r => (format' f).encode


||| Convert an inductive-recursive format description to an indexed format
indRecToIndexed : (f : IndRec.Format) -> Indexed.FormatOf f.Rep
indRecToIndexed End = Indexed.End
indRecToIndexed Fail = Indexed.Fail
indRecToIndexed (Pure x) = Indexed.Pure x
indRecToIndexed (Ignore f def) = Indexed.Ignore (indRecToIndexed f) def
indRecToIndexed (Repeat len f) = Indexed.Repeat len (indRecToIndexed f)
indRecToIndexed (Tuple fs) = ?todo_indRecToIndexedTuple
indRecToIndexed (Pair f1 f2) = Indexed.Pair (indRecToIndexed f1) (indRecToIndexed f2)
indRecToIndexed (Bind f g) = Indexed.Bind (indRecToIndexed f) (\x => indRecToIndexed (g x))


mutual

  ||| Convert an indexed format description to an inductive-recursive format
  indexedToIndRecFormat : (f : Indexed.Format) -> (f' : IndRec.Format ** f.Rep = f'.Rep)
  indexedToIndRecFormat (MkFormat () End) = (End ** Refl)
  indexedToIndRecFormat (MkFormat Void Fail) = (Fail ** Refl)
  indexedToIndRecFormat (MkFormat (Sing x) (Pure x)) = (Pure x ** Refl)
  indexedToIndRecFormat (MkFormat () (Ignore f def)) with (indexedToIndRecFormatOf f)
    _ | MkFormatOf f' = (Ignore f' def ** Refl)
  indexedToIndRecFormat (MkFormat (Vect len _) (Repeat len f)) with (indexedToIndRecFormatOf f)
    _ | MkFormatOf f' = (Repeat len f' ** Refl)
  indexedToIndRecFormat (MkFormat (HVect reps) (Tuple fs)) =
    ?todo_indexedToIndRecFormatTuple
  indexedToIndRecFormat (MkFormat (_, _) (Pair f1 f2)) with (indexedToIndRecFormatOf f1, indexedToIndRecFormatOf f2)
    _ | (MkFormatOf f1', MkFormatOf f2') = (Pair f1' f2' ** Refl)
  indexedToIndRecFormat (MkFormat (x : _ ** _) (Bind f1 f2)) with (indexedToIndRecFormatOf f1)
    _ | MkFormatOf f1' =
      (Bind f1' (\x => ?indexedToIndRecFormatBind_f2) ** ?todoBindPrf)


  ||| Convert an indexed format description to an inductive-recursive format
  indexedToIndRecFormatOf : {0 A : Type} -> (f : Indexed.FormatOf A) -> IndRec.FormatOf A
  indexedToIndRecFormatOf End = MkFormatOf End
  indexedToIndRecFormatOf Fail = MkFormatOf Fail
  indexedToIndRecFormatOf (Pure x) = MkFormatOf (Pure x)
  indexedToIndRecFormatOf (Ignore f def) with (indexedToIndRecFormatOf f)
    _ | MkFormatOf f' = MkFormatOf (Ignore f' def)
  indexedToIndRecFormatOf (Repeat len f) with (indexedToIndRecFormatOf f)
    _ | MkFormatOf f' = MkFormatOf (Repeat len f')
  indexedToIndRecFormatOf (Tuple fs) =
    ?todo_indexedToIndRecFormatOfTuple
  indexedToIndRecFormatOf (Pair f1 f2) with (indexedToIndRecFormatOf f1, indexedToIndRecFormatOf f2)
    _ | (MkFormatOf f1', MkFormatOf f2') = MkFormatOf (Pair f1' f2')
  indexedToIndRecFormatOf (Bind f1 f2) with (indexedToIndRecFormatOf f1)
    _ | MkFormatOf f1' =
  --     -- let
  --     --   bindF1F2 = Bind f1' (\x =>
  --     --     let
  --     --       (f2' ** _) = toFormat (indexedToIndRecFormatOf (f2 x))
  --     --     in
  --     --       ?indexedToIndRecFormatOfBind_f2)
  --     -- in
  --     -- ?indexedToIndRecFormatOfBind
  --     MkFormatOf (Bind f1' (\x =>
  --         let
  --           (f2' ** prf) = toFormat (indexedToIndRecFormatOf (f2 x))
  --         in
  --           ?indexedToIndRecFormatOfBind_f2))
  -- indexedToIndRecFormatOf (Bind f1 f2) with (indexedToIndRecFormat (MkFormat _ f1))
  --   _ | (f1' ** prf) =
      ?indexedToIndRecFormatOfBind


-- ||| Convert an indexed format description to an inductive-recursive format
-- indexedToIndRec : {0 A : Type} -> (f : Indexed.FormatOf A) -> IndRec.FormatOf A
-- indexedToIndRec End = MkFormatOf IndRec.End
-- indexedToIndRec Fail = MkFormatOf IndRec.Fail
-- indexedToIndRec (Pure x) = MkFormatOf (IndRec.Pure x)
-- indexedToIndRec (Ignore f def) with (indexedToIndRec f)
--   indexedToIndRec (Ignore _ def) | MkFormatOf f = MkFormatOf (IndRec.Ignore f def)
-- indexedToIndRec (Repeat len f) with (indexedToIndRec f)
--   indexedToIndRec (Repeat len _) | MkFormatOf f = MkFormatOf (IndRec.Repeat len f)
-- indexedToIndRec (Bind f1 f2) with (indexedToIndRec f1)
--   indexedToIndRec (Bind _ f2) | MkFormatOf f1 =
--     ?todo_indexedToIndRec

-- indexedToIndRec (Bind f1 f2) with (indexedToIndRec f1)
  -- _ | (MkFormatOf End) = MkFormatOf (Bind End ?todo_indexedToIndRec_2)
  -- _ | (MkFormatOf Fail) = MkFormatOf (Bind Fail absurd)
  -- _ | (MkFormatOf (Pure f)) = MkFormatOf (Bind ?todo_indexedToIndRec_4)
  -- _ | (MkFormatOf (Ignore f def)) = MkFormatOf (Bind ?todo_indexedToIndRec_5)
  -- _ | (MkFormatOf (Repeat k x)) = MkFormatOf (Bind ?todo_indexedToIndRec_6)
  -- _ | (MkFormatOf (Bind f g)) = MkFormatOf (Bind ?todo_indexedToIndRec_7)

-- indexedToIndRec (Bind f1 f2) with (sameRep (indexedToIndRec f1))
--   indexedToIndRec (Bind _ f2) | (f1' ** prf) =
--     rewrite sym prf in MkFormatOf (Bind f1' (\x => ?todo_indexedToIndRec))
--     where
--       indexedToIndRecF2 : {0 A : Type} -> {0 B : A -> Type} -> ((x : A) -> Indexed.FormatOf (B x)) -> ((x : A) -> IndRec.FormatOf (B x))
--       indexedToIndRecF2 x = ?todofF2

-- indexedToIndRec (Bind f1 f2) with (indexedToIndRec f1)
--   indexedToIndRec (Bind _ f2) | MkFormatOf f1 =
--     let
--       bindF1 = Bind f1
--       bodyF2 : x : f1.Rep -> FormatOf ()
--       bodyF2 = x : f1.Rep =>
--         case sameRep (indexedToIndRec (f2 x)) of
--           (f2' ** prf) => f2')
--     in
--     ?todo_indexedToIndRec
--     where
--       indexedToIndRecF2 : {0 A : Type} -> {0 B : A -> Type} -> ((x : A) -> Indexed.FormatOf (B x)) -> ((x : A) -> IndRec.FormatOf (B x))
--       indexedToIndRecF2 x = ?todofF2

-- indexedToIndRec (Bind f1 f2) with (sameRep (indexedToIndRec f1))
--   _ | (End ** prf) = let bindF1 = Bind f1 in ?todo_indexedToIndRec_2
--   _ | (Fail ** prf) = let bindF1 = Bind f1 in ?todo_indexedToIndRec_3
--   _ | ((Pure f) ** prf) = let bindF1 = Bind f1 in ?todo_indexedToIndRec_4
--   _ | ((Ignore f def) ** prf) = let bindF1 = Bind f1 in ?todo_indexedToIndRec_5
--   _ | ((Repeat k x) ** prf) = let bindF1 = Bind f1 in ?todo_indexedToIndRec_6
--   _ | ((Bind f g) ** prf) = let bindF1 = Bind f1 in ?todo_indexedToIndRec_7

-- indexedToIndRec (Bind f1 f2) with (sameRep (indexedToIndRec f1))
--   indexedToIndRec (Bind _ f2) | (f1' ** prf) =
--     let bindF1 = Bind f1' in
    -- MkFormatOf (Bind f1' (\x =>
    --   let
    --     f2' = f2 x
    --     -- f2'' = sameRep f2'
    --   in
    --     ?help))
  -- let f1' = indexedToIndRec f1
  --     (f1'', fromRep) = sameRep' f1'
  -- in

-- indexedToIndRec (Bind f1 f2) = indexedToIndRecBind f1 f2
-- indexedToIndRec (Bind f1 f2) with (sameRep (indexedToIndRec f1))
--   indexedToIndRec (Bind _ f2) | (f1 ** prf) =
--     let hmm = Bind f1
--     in
--     --    f1 : Format
--     --  0 A : Type
--     --    f2 : (x : f1.Rep) -> FormatOf (B x)
--     -- ------------------------------
--     -- todo_indexedToIndRec : FormatOf (DPair (Rep f1) (\x => B x))
    -- ?todo_indexedToIndRec
    -- MkFormatOf (Bind f1 (\x => ?help))
--     --   let
--     --   --   x' : A
--     --   --   x' = x
--     --     f2' = f2 x
--     --     MkFormatOf f2'' = indexedToIndRec f2'
--     --   in
--     --   f2''))

-- indexedToIndRec' : {0 A : Type} -> (f : Indexed.FormatOf A) -> IndRec.Format
-- indexedToIndRec' End = IndRec.End
-- indexedToIndRec' Fail = IndRec.Fail
-- indexedToIndRec' (Pure x) = IndRec.Pure x
-- indexedToIndRec' (Ignore f def) with (MkFormatOf (indexedToIndRec' f))
--   _ | f' = IndRec.Ignore (indexedToIndRec' f) ?todo1
-- indexedToIndRec' (Repeat len f) = IndRec.Repeat len (indexedToIndRec' f)
-- indexedToIndRec' (Bind f1 f2) = IndRec.Bind (indexedToIndRec' f1) ?todo2


-- indexedToIndRec'' : (f : Indexed.Format) -> IndRec.Format
-- indexedToIndRec'' (MkFormat () End) = IndRec.End
-- indexedToIndRec'' (MkFormat Void Fail) = IndRec.Fail
-- indexedToIndRec'' (MkFormat (Sing x) (Pure x)) = IndRec.Pure x
-- indexedToIndRec'' (MkFormat () (Ignore f def)) with (indexedToIndRec'' (MkFormat _ f))
--   _ | f'' = IndRec.Ignore f'' ?tododef
-- indexedToIndRec'' (MkFormat rep (Repeat len f)) = IndRec.Repeat len (indexedToIndRec'' f)
-- indexedToIndRec'' (MkFormat rep (Bind f1 f2)) = IndRec.Bind (indexedToIndRec'' f1) ?todo2


-- Reproduction of difficulties in OpenType format, drawing parallels to
-- Tarski-style universes.

record Flag where
  constructor MkFlag
  id : Nat
  repeat : case id of
    0 => Nat
    S n => Sing {A = Nat} 0

record SimpleGlyph where
  constructor MkSimpleGlyph
  flag : Flag
  flag_repeat : Sing {A = Nat} (case flag of
    MkFlag 0 repeat => repeat
    MkFlag (S n) repeat => val repeat)

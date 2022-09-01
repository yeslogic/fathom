||| Isomorphisms between types

module Fathom.Data.Iso


||| A datatype that forms a proof that two types are isomorphic to each other,
||| ie. can be converted to and from each other while preserving information.
public export
record Iso (0 A : Type) (0 B : Type) where
  constructor MkIso
  to : A -> B
  from : B -> A
  0 toFrom : (y : B) -> to (from y) = y
  0 fromTo : (x : A) -> from (to x) = x


sym : Iso a b -> Iso b a
sym iso = MkIso
  { to = iso.from
  , from = iso.to
  , toFrom = iso.fromTo
  , fromTo = iso.toFrom
  }


isoSym : Iso (Iso a b) (Iso b a)
isoSym = MkIso
  { to = sym
  , from = sym
  , toFrom = \(MkIso _ _ _ _) => Refl
  , fromTo = \(MkIso _ _ _ _) => Refl
  }


cong : (f : Type -> Type) -> Iso a b -> Iso (f a) (f b)
cong f iso = MkIso
  { to = \fa => ?todo_to
  , from = \fb => ?todo_from
  , toFrom = ?todo_toFrom
  , fromTo = ?todo_fromTo
  }


trans : Iso a b -> Iso b c -> Iso a c
trans isoAB isoBC = MkIso
  { to = isoBC.to . isoAB.to
  , from = isoAB.from . isoBC.from
  , toFrom = \c => trans ?todo_toFrom1' (isoBC.toFrom c)
  , fromTo = \c => ?todo_fromTo'
  }

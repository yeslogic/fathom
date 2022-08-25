module Fathom.Data.Refine


||| A value that is refined by a proposition.
|||
||| The proof of the proposition is erased at runtime.
|||
||| This is a bit like `(x : A ** B)`, but with the second element erased.
public export
record Refine (0 A : Type) (0 P : A -> Type) where
  constructor MkRefine
  ||| The refined value
  val : A
  ||| The a proof that @val is refined by @P
  {auto 0 prf : P val}

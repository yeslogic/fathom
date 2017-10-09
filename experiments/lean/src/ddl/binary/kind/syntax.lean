namespace ddl.binary

  /- Kinds of types in the binary language -/
  inductive kind : Type
    | type : kind
    | arrow : kind → kind → kind

  notation `★` := kind.type
  notation k₁ ` ⇒ ` k₂ := kind.arrow k₁ k₂

end ddl.binary

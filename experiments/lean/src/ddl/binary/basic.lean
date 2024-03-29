import ddl.host.basic

namespace ddl.binary

  open ddl


  /- Kinds of types in the binary language -/
  inductive kind : Type
    | type : kind
    | arrow : kind → kind → kind

  notation k₁ ` ⇒ ` k₂ := kind.arrow k₁ k₂


  /- The type system of the binary language -/
  inductive type (ℓ α : Type) : Type
    | bvar {} : ℕ → type
    | fvar {} : α → type
    | bit {} : type
    | union_nil {} : type
    | union_cons : ℓ → type → type → type
    | struct_nil {} : type
    | struct_cons : ℓ → type → type → type
    | array : type → host.expr ℓ → type
    | assert : type → host.expr ℓ → type
    | interp : type → host.expr ℓ → host.type ℓ → type
    | lam : kind → type → type
    | app : type → type → type


  -- Application operator
  infixl ` ∙ `:50 := type.app

  namespace type

    variables {ℓ α : Type}

    instance has_coe_from_nat : has_coe ℕ (type ℓ α) := ⟨bvar⟩
    instance has_coe_from_atom : has_coe α (type ℓ α) := ⟨fvar⟩

    def lookup (l : ℓ) [decidable_eq ℓ] : type ℓ α → option (type ℓ α)
      | (struct_cons l' t tr) := if l = l' then some t else lookup tr
      | _                     := none

  end type

end ddl.binary

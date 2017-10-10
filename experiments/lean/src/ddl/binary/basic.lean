import ddl.host.basic

namespace ddl.binary

  open ddl


  /- Kinds of types in the binary language -/
  inductive kind : Type
    | type : kind
    | arrow : kind → kind → kind

  notation `★` := kind.type
  notation k₁ ` ⇒ ` k₂ := kind.arrow k₁ k₂


  /- The type system of the binary language -/
  inductive type (ℓ α : Type) : Type
    | bvar {} : ℕ → type
    | fvar {} : α → type
    | bit {} : type
    | sum : type → type → type
    | struct_nil {} : type
    | struct_cons : ℓ → type → type → type
    | array : type → host.expr ℓ → type
    | cond : type → host.expr ℓ → type
    | abs : kind → type → type
    | app : type → type → type


  -- Abstraction and conditional type notation - note that we are a
  -- using nameless for identifiers encoding so we don't include the argument
  -- identifiers
  notation `Λ0: ` k `, ` t := type.abs k t
  notation `{0: ` t ` | ` e ` }` := type.cond t e
  -- Array type syntax
  notation `[ ` t `; ` e ` ]` := type.array t e
  -- Application operator
  infixl ` ∙ `:50 := type.app

  namespace type

    variables {ℓ α : Type}

    instance has_coe_from_nat : has_coe ℕ (type ℓ α) := ⟨bvar⟩
    instance has_coe_from_atom : has_coe α (type ℓ α) := ⟨fvar⟩

    -- Overload the `+` operator for constructing sum types
    instance : has_add (type α ℓ) := ⟨type.sum⟩


    def lookup (l : ℓ) [decidable_eq ℓ] : type ℓ α → option (type ℓ α)
      | (struct_cons l' t tr) := if l = l' then some t else lookup tr
      | _                     := none

  end type

end ddl.binary

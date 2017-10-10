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
  inductive type (α : Type) : Type
    | bvar {} : ℕ → type
    | fvar : α → type
    | unit {} : type
    | bit {} : type
    | sum : type → type → type
    | prod : type → type → type
    | array : type → host.expr → type
    | cond : type → host.expr → type
    | abs : kind → type → type
    | app : type → type → type

  -- Product, abstraction and conditional type notation - note that we are a
  -- using nameless for identifiers encoding so we don't include the argument
  -- identifiers
  notation `Σ0: ` t₁ `, ` t₂ := type.prod t₁ t₂
  notation `Λ0: ` k `, ` t := type.abs k t
  notation `{0: ` t ` | ` e ` }` := type.cond t e
  -- Array type syntax
  notation `[ ` t `; ` e ` ]` := type.array t e
  -- Application operator
  infixl ` ∙ `:50 := type.app

  namespace type

    variables {α : Type}

    instance has_coe_from_nat : has_coe ℕ (type α) := ⟨bvar⟩
    instance has_coe_from_atom : has_coe α (type α) := ⟨fvar⟩

    -- Overload the `+` operator for constructing sum types
    instance : has_add (type α) := ⟨type.sum⟩

  end type

end ddl.binary

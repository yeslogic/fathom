/- Embedding as Lean terms -/

import ddl.binary.basic
import ddl.binary.kinding

namespace ddl

  namespace host

    open ddl.host

    variables {ℓ : Type} [decidable_eq ℓ]

    def type.embed : type ℓ → Type
      | type.bool := bool
      | (type.arith _) := ℤ
      | type.union_nil := empty
      | (type.union_cons _ t₁ t₂) := t₁.embed ⊕ t₂.embed
      | type.struct_nil := unit
      | (type.struct_cons _ t₁ t₂) := t₁.embed × t₂.embed
      | (type.array t₁) := list t₁.embed

    def typed_expr.embed : Π (e : typed_expr ℓ), e.t.embed
      | ⟨expr.bool b,                      type.bool,       h⟩ := b
      | ⟨expr.arith n,                     (type.arith _),  h⟩ := sorry
      | ⟨expr.binop arith.binop.add e₁ e₂, (type.arith _),  h⟩ := sorry
      | ⟨expr.binop arith.binop.mul e₁ e₂, (type.arith _),  h⟩ := sorry
      | ⟨_,                                _,               _⟩ := sorry -- hmmm...

  end host

  namespace binary

    open ddl.binary

    def kind.embed : kind → Type 1
      | kind.type := Type 0
      | (kind.arrow k₁ k₂) := kind.embed k₁ → kind.embed k₂

    def kinded_type.embed {ℓ α} [decidable_eq ℓ] : Π (kt : kinded_type ℓ α), kt.k.embed :=
      sorry

  end binary

end ddl

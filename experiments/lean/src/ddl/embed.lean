/- Embedding as Lean terms -/

import ddl.binary.kind.syntax
import ddl.binary.type.semantics

namespace ddl

  namespace host

    open ddl.host

    def type.embed : type → Type
      | type.unit := unit
      | type.bool := bool
      | type.nat := ℕ
      | (type.sum t₁ t₂) := t₁.embed ⊕ t₂.embed
      | (type.prod t₁ t₂) := t₁.embed × t₂.embed
      | (type.array t₁) := list t₁.embed

    def typed_expr.embed : Π (e : typed_expr), e.t.embed
      | ⟨expr.bool b,                    type.bool, h⟩ := b
      | ⟨expr.nat n,                     type.nat,  h⟩ := n
      | ⟨expr.app_binop binop.add e₁ e₂, type.nat,  h⟩ := sorry
      | ⟨expr.app_binop binop.mul e₁ e₂, type.nat,  h⟩ := sorry
      | ⟨_,                              _,         _⟩ := sorry -- hmmm...

  end host

  namespace binary

    open ddl.binary

    def kind.embed : kind → Type 1
      | kind.type := Type 0
      | (kind.arrow k₁ k₂) := kind.embed k₁ → kind.embed k₂

    def type.kinded.embed {α : Type} : Π (tk : type.kinded α), tk.k.embed :=
      sorry

  end binary

end ddl

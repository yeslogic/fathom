import ddl.binary.basic
import ddl.binary.repr
import ddl.binder
import ddl.ctx
import ddl.host.typing

namespace ddl.binary

  open ddl
  open ddl.binary

  section

    variables {ℓ α : Type} [decidable_eq ℓ]

    inductive has_kind : ctx ℓ → type ℓ α → kind → Prop
      | bvar {Γ} (x : ℕ) {k} :
          ctx.lookup x Γ = some (binder.lam k) →
          has_kind Γ ↑x k
      | bit {Γ} :
          has_kind Γ type.bit kind.type
      | union_nil {Γ} :
          has_kind Γ type.union_nil kind.type
      | union_cons {Γ l t₁ t₂} :
          has_kind Γ t₁ kind.type →
          has_kind Γ t₂ kind.type →
          has_kind Γ (type.union_cons l t₁ t₂) kind.type
      | struct_nil {Γ} :
          has_kind Γ type.struct_nil kind.type
      | struct_cons {Γ l t₁ t₂} :
          has_kind Γ t₁ kind.type →
          has_kind (binder.struct t₁.repr :: Γ) t₂ kind.type →
          has_kind Γ (type.struct_cons l t₁ t₂) kind.type
      | array {Γ t e} :
          has_kind Γ t kind.type →
          host.has_type e host.type.nat →
          has_kind Γ (type.array t e) kind.type
      | assert {Γ t e} :
          has_kind Γ t kind.type →
          host.has_type /- FIXME: add binding? -/ e host.type.bool →
          has_kind Γ (type.assert t e) kind.type
      | interp {Γ t e th} :
          has_kind Γ t kind.type →
          host.has_type /- FIXME: add binding? -/ e th →
          has_kind Γ (type.interp t e th) kind.type
      | lam {Γ t k₁ k₂} :
          has_kind (binder.lam k₁ :: Γ) t k₁ →
          has_kind Γ (type.lam k₁ t) (k₁ ⇒ k₂)
      | app {Γ t₁ t₂ k₁ k₂} :
          has_kind Γ t₁ (k₁ ⇒ k₂) →
          has_kind Γ t₂ k₁ →
          has_kind Γ (t₁ ∙ t₂) k₂

  end


  /- A correctly kinded type -/
  structure kinded_type (ℓ α : Type) [decidable_eq ℓ] : Type :=
    (Γ : ctx ℓ)
    (t : type ℓ α)
    (k : kind)
    (h : has_kind Γ t k)

end ddl.binary

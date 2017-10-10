import ddl.host.typing
import ddl.binary.basic
import ddl.binary.binder
import ddl.binary.ctx
import ddl.binary.repr

namespace ddl.binary

  open ddl
  open ddl.binary

  section

    variables {ℓ α : Type} [decidable_eq ℓ]

    inductive has_kind : ctx ℓ → type ℓ α → kind → Prop
      | bvar {Γ} (x : ℕ) {k} :
          ctx.lookup x Γ = some (binder.abs k) →
          has_kind Γ ↑x k
      | bit {Γ} :
          has_kind Γ type.bit ★
      | sum {Γ t₁ t₂} :
          has_kind Γ t₁ ★ →
          has_kind Γ t₂ ★ →
          has_kind Γ (t₁ + t₂) ★
      | struct_nil {Γ} :
          has_kind Γ type.struct_nil ★
      | struct_cons {Γ l t₁ t₂} :
          has_kind Γ t₁ ★ →
          has_kind (Σ ⟦ t₂ ⟧ :: Γ) t₂ ★ →
          has_kind Γ (type.struct_cons l t₁ t₂) ★
      | array {Γ t e} :
          has_kind Γ t ★ →
          host.has_type e host.type.nat →
          has_kind Γ [ t; e ] ★
      | cond {Γ t e} :
          has_kind Γ t ★ →
          host.has_type e host.type.bool →
          has_kind Γ {0: t | e } ★
      | abs {Γ t k₁ k₂} :
          has_kind (Λ k₁ :: Γ) t k₁ →
          has_kind Γ (Λ0: k₁, t) (k₁ ⇒ k₂)
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

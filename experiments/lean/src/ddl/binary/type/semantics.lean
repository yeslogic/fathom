import ddl.binary.binder
import ddl.binary.ctx
import ddl.binary.kind.syntax
import ddl.binary.type.syntax
import ddl.binary.type.representation

namespace ddl.binary.type

  open ddl
  open ddl.binary

  variables {α : Type}

  inductive has_kind : ctx → type α → kind → Prop
    | var {Γ} (x : ℕ) {k} :
        ctx.lookup x Γ = some (binder.abs k) →
        has_kind Γ x k
    | unit {Γ} :
        has_kind Γ type.unit ★
    | bit {Γ} :
        has_kind Γ type.bit ★
    | sum {Γ t₁ t₂} :
        has_kind Γ t₁ ★ →
        has_kind Γ t₂ ★ →
        has_kind Γ (t₁ + t₂) ★
    | prod {Γ t₁ t₂} :
        has_kind Γ t₁ ★ →
        has_kind (Σ ⟦ t₂ ⟧ :: Γ) t₂ ★ →
        has_kind Γ (Σ0: t₁, t₂) ★
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

  /- A correctly kinded type -/
  structure kinded (α : Type) : Type :=
    (Γ : ctx)
    (t : type α)
    (k : kind)
    (h : has_kind Γ t k)

end ddl.binary.type

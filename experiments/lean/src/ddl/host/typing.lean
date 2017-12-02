import ddl.host.basic

namespace ddl.host

  open ddl

  section

    variables {ℓ : Type} [decidable_eq ℓ]

    inductive has_type : expr ℓ → type ℓ → Prop
      | bool {b} :
          has_type (expr.bool b) type.bool
      | arith {ae₁ at₁} :
          arith.has_type ae₁ at₁ →
          has_type (expr.arith ae₁) (type.arith at₁)
      | neg {e₁} :
          has_type e₁ (type.arith sorry) →
          has_type (-e₁) (type.arith sorry)
      | add {e₁ e₂} :
          has_type e₁ (type.arith sorry) →
          has_type e₂ (type.arith sorry) →
          has_type (e₁ + e₂) (type.arith sorry)
      | sub {e₁ e₂} :
          has_type e₁ (type.arith sorry) →
          has_type e₂ (type.arith sorry) →
          has_type (e₁ - e₂) (type.arith sorry)
      | mul {e₁ e₂} :
          has_type e₁ (type.arith sorry) →
          has_type e₂ (type.arith sorry) →
          has_type (e₁ * e₂) (type.arith sorry)
      | proj {e₁ tr tf l} :
          has_type e₁ tr →
          type.lookup l tr = some tf →
          has_type (expr.proj e₁ l) tf

  end

  /- A correctly typed expression -/
  structure typed_expr (ℓ : Type) [decidable_eq ℓ] : Type :=
    (e : expr ℓ)
    (t : type ℓ)
    (h : has_type e t)

end ddl.host

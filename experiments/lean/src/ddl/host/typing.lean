import ddl.host.basic

namespace ddl.host

  open ddl

  section

    variables {ℓ : Type} [decidable_eq ℓ]

    inductive has_type : expr ℓ → type ℓ → Prop
      | bool {b} :
          has_type (expr.bool b) type.bool
      | nat {n} :
          has_type (expr.nat n) type.nat
      | add {e₁ e₂} :
          has_type e₁ type.nat →
          has_type e₂ type.nat →
          has_type (e₁ + e₂) type.nat
      | mul {e₁ e₂} :
          has_type e₁ type.nat →
          has_type e₂ type.nat →
          has_type (e₁ * e₂) type.nat
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

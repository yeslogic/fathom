import ddl.host.basic

namespace ddl.host

  inductive has_type : expr → type → Prop
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


  /- A correctly typed expression -/
  structure typed_expr : Type :=
    (e : expr)
    (t : type)
    (h : has_type e t)

end ddl.host

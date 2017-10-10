import ddl.host.basic

namespace ddl.host

  variables {ℓ : Type}


  /- 'Stuck' values -/
  inductive value : expr ℓ → Prop
    | bool (bv : bool) : value (expr.bool bv)
    | nat (nv : ℕ) : value (expr.nat nv)


  reserve infixl ` ⟹ `:50
  reserve infixl ` ⟹* `:50

  inductive step : expr ℓ → expr ℓ → Prop
    infixl ` ⟹ ` := step

    | value {e} :
        value e →
        e ⟹ e
    | binop_rec_l {op e₁ e₁' e₂} :
        e₁ ⟹ e₁' →
        expr.app_binop op e₁ e₂ ⟹ expr.app_binop op e₁' e₂
    | binop_rec_r {op e₁ e₂ e₂'} :
        value e₁ →
        e₂ ⟹ e₂' →
        expr.app_binop op e₁ e₂ ⟹ expr.app_binop op e₁ e₂'
    | binop_add {nv₁ nv₂} :
        expr.nat nv₁ + expr.nat nv₂ ⟹ expr.nat (nv₁ + nv₂)
    | binop_mul {nv₁ nv₂} :
        expr.nat nv₁ * expr.nat nv₂ ⟹ expr.nat (nv₁ * nv₂)

  infixl ` ⟹ ` := step
  infixl ` ⟹* ` := ddl.multi step

end ddl.host

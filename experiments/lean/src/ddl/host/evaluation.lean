import ddl.host.basic

namespace ddl.host

  variables {ℓ : Type}


  /- 'Stuck' values -/
  inductive value : expr ℓ → Prop
    | bool (bv : bool) : value (expr.bool bv)
    | arith (ae : arith.expr) : arith.value ae → value (expr.arith ae)


  reserve infixl ` ⟹ `:50
  reserve infixl ` ⟹* `:50

  inductive step : expr ℓ → expr ℓ → Prop
    infixl ` ⟹ ` := step

    | value {e} :
        value e →
        e ⟹ e
    | unnop_neg {n₁} :
        -(expr.arith n₁) ⟹ expr.arith (-n₁)
    | binop_rec_l {op e₁ e₁' e₂} :
        e₁ ⟹ e₁' →
        expr.binop op e₁ e₂ ⟹ expr.binop op e₁' e₂
    | binop_rec_r {op e₁ e₂ e₂'} :
        value e₁ →
        e₂ ⟹ e₂' →
        expr.binop op e₁ e₂ ⟹ expr.binop op e₁ e₂'
    | binop_add {nv₁ nv₂} :
        expr.arith nv₁ + expr.arith nv₂ ⟹ expr.arith (nv₁ + nv₂)
    | binop_sub {nv₁ nv₂} :
        expr.arith nv₁ - expr.arith nv₂ ⟹ expr.arith (nv₁ - nv₂)
    | binop_mul {nv₁ nv₂} :
        expr.arith nv₁ * expr.arith nv₂ ⟹ expr.arith (nv₁ * nv₂)

  infixl ` ⟹ ` := step
  infixl ` ⟹* ` := ddl.multi step

end ddl.host

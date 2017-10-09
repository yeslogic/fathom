import ddl.basic

namespace ddl.host

  /- The type syntax of the host language -/
  inductive type : Type
    | unit : type
    | bool : type
    | nat : type
    | sum : type → type → type
    | prod : type → type → type
    | array : type → type

  namespace type

    instance : has_add type := ⟨type.sum⟩
    instance : has_mul type := ⟨type.prod⟩

  end type


  /- Binary operators -/
  inductive binop : Type
    | add
    | mul


  /- The expression syntax of the host language -/
  inductive expr : Type
    | bool : bool → expr
    | nat : ℕ → expr
    | app_binop : binop → expr → expr → expr

  instance has_coe_to_bool : has_coe bool expr := ⟨expr.bool⟩
  instance has_coe_to_nat : has_coe ℕ expr  := ⟨expr.nat⟩

  namespace expr

    instance : has_add expr := ⟨app_binop binop.add⟩
    instance : has_mul expr := ⟨app_binop binop.mul⟩

  end expr


  /- 'Stuck' values -/
  inductive value : expr → Prop
    | bool (bv : bool) : value (expr.bool bv)
    | nat (nv : ℕ) : value (expr.nat nv)


  -- TYPING RULES

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


  -- EMBEDDING

  /- embed a host type into Lean -/
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


  -- EVALUATION RULES

  reserve infixl ` ⟹ `:50
  reserve infixl ` ⟹* `:50

  inductive step : expr → expr → Prop
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

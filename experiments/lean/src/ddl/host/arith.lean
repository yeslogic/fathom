/- A sublanguage of bounded integer arithmetic expressions -/
namespace ddl.host.arith

  /- Unary operators -/
  inductive unop : Type
    | neg : unop

  /- Binary operators -/
  inductive binop : Type
    | add : binop
    | sub : binop
    | mul : binop

  /- Integer arithmetic expressions -/
  inductive expr : Type
    | const : ℤ → expr
    | unop : unop → expr → expr
    | binop : binop → expr → expr → expr

  namespace expr

    instance : has_coe ℤ expr := ⟨const⟩
    instance : has_neg expr := ⟨unop unop.neg⟩
    instance : has_add expr := ⟨binop binop.add⟩
    instance : has_sub expr := ⟨binop binop.sub⟩
    instance : has_mul expr := ⟨binop binop.mul⟩

  end expr


  /- Bounded integer arithemetic types -/
  structure type : Type :=
    (min : ℤ)
    (max : ℤ)
    (min_le_max : min ≤ max)

  namespace type

    def const (n : ℤ) : type :=
      { min := n
      , max := n
      , min_le_max := by refl
      }

    private lemma neg_le_int {a b : ℤ} (h : b ≤ a) : -a ≤ -b :=
      by admit

    def neg (t₁ : type) : type :=
      { min := -t₁.max
      , max := -t₁.min
      , min_le_max := neg_le_int t₁.min_le_max
      }

    private lemma add_le_int {a₁ a₂ b₁ b₂ : ℤ} (h₁ : a₁ ≤ b₁) (h₂ : a₂ ≤ b₂) : a₁ + a₂ ≤ b₁ + b₂ :=
      by admit

    def add (t₁ t₂ : type) : type :=
      { min := t₁.min + t₂.min
      , max := t₁.max + t₂.max
      , min_le_max := add_le_int t₁.min_le_max t₂.min_le_max
      }

    private lemma sub_le_int {a₁ a₂ b₁ b₂ : ℤ} (h₁ : a₁ ≤ b₁) (h₂ : a₂ ≤ b₂) : a₁ - a₂ ≤ b₁ - b₂ :=
      by admit

    def sub (t₁ t₂ : type) : type :=
      { min := t₁.min - t₂.min
      , max := t₁.max - t₂.max
      , min_le_max := sub_le_int t₁.min_le_max t₂.min_le_max
      }

    def mul (t₁ t₂ : type) : type :=
      sorry

    instance : has_neg type := ⟨neg⟩
    instance : has_add type := ⟨add⟩
    instance : has_sub type := ⟨sub⟩
    instance : has_mul type := ⟨mul⟩

  end type


  /- Typing relation for integer arithmetic -/
  inductive has_type : expr → type → Type
    | const {n : ℤ} :
        has_type n (type.const n)
    | unop_neg {e₁ t₁} :
        has_type e₁ t₁ →
        has_type (-e₁) (-t₁)
    | binop_add {e₁ e₂ t₁ t₂} :
        has_type e₁ t₁ →
        has_type e₂ t₂ →
        has_type (e₁ + e₂) (t₁ + t₂)
    | binop_sub {e₁ e₂ t₁ t₂} :
        has_type e₁ t₁ →
        has_type e₂ t₂ →
        has_type (e₁ - e₂) (t₁ - t₂)
    | binop_mul {e₁ e₂ t₁ t₂} :
        has_type e₁ t₁ →
        has_type e₂ t₂ →
        has_type (e₁ * e₂) (t₁ * t₂)
    | subsume {e₁} {t₁ t₁' : type} :
        has_type e₁ t₁ →
        t₁'.min ≤ t₁.min →
        t₁.max ≤ t₁'.max →
        has_type e₁ t₁'
    | coerce {e₁} {t₁ t₁' : type} :
        has_type e₁ t₁ →
        t₁.min ≤ t₁'.min →
        t₁'.max ≤ t₁.max →
        has_type e₁ t₁'

  /- Small step evaluation -/

  inductive value : expr → Prop
    | const (n : ℤ) : value (expr.const n)

  reserve infixl ` ⟹ `:50

  inductive step : expr → expr → Prop
    infixl ` ⟹ ` := step

    | value {e} :
        value e →
        e ⟹ e
    | unnop_neg {n₁} :
        -(expr.const n₁) ⟹ expr.const (-n₁)
    | unop_rec {op e₁ e₁'} :
        e₁ ⟹ e₁' →
        expr.unop op e₁ ⟹ expr.unop op e₁'
    | binop_rec_l {op e₁ e₁' e₂} :
        e₁ ⟹ e₁' →
        expr.binop op e₁ e₂ ⟹ expr.binop op e₁' e₂
    | binop_rec_r {op e₁ e₂ e₂'} :
        value e₁ →
        e₂ ⟹ e₂' →
        expr.binop op e₁ e₂ ⟹ expr.binop op e₁ e₂'
    | binop_add {n₁ n₂} :
        expr.const n₁ + expr.const n₂ ⟹ expr.const (n₁ + n₂)
    | binop_sub {n₁ n₂} :
        expr.const n₁ - expr.const n₂ ⟹ expr.const (n₁ - n₂)
    | binop_mul {n₁ n₂} :
        expr.const n₁ * expr.const n₂ ⟹ expr.const (n₁ * n₂)

  infixl ` ⟹ ` := step

  -- PROGRESS
  -- https://softwarefoundations.cis.upenn.edu/plf-current/StlcProp.html#lab220

  theorem progress :
      Π (e : expr) (t : type),
      has_type e t →
      value e ∨ ∃ e', e ⟹ e' :=
    begin
      intros e t ht,
      -- TODO
      admit
    end


  -- PRESERVATION
  -- https://softwarefoundations.cis.upenn.edu/plf-current/StlcProp.html#lab222

  theorem preservation :
      Π (e e' : expr) (t : type),
      has_type e t →
      e ⟹ e' →
      has_type e' t :=
    begin
      intros e e' t ht hs,
      -- TODO
      admit
    end

end ddl.host.arith

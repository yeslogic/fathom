namespace ddl

  -- SYNTAX

  inductive op : Type
    | add
    | mul

  inductive value : Type
    | bool : bool → value
    | nat : ℕ → value

  mutual inductive expr, type, kind
    with expr : Type
      | const : value → expr
      | app_op : op → expr → expr → expr
      | var : string → expr
      | proj : expr → string → expr
      | index : expr → expr → expr

    with type : Type
      | bool : type
      | nat : type
      | u8 : type
      | var : string → type
      | abs : string → kind → type → type
      | app : type → type → type
      | sum : type → type → type
      | struct : string → type → type → type
      | array : type → expr → type

    with kind : Type
      | host : kind
      | binary : kind
      | arrow : kind → kind → kind

  infix + := (expr.app_op op.add)
  infix * := (expr.app_op op.mul)
  notation `Λ` x `:` κ `,` τ := type.abs x κ τ
  notation κ₁ ` ↣ ` κ₂ := kind.arrow κ₁ κ₂

  -- Defined for fully evaluated expressions
  inductive is_value : expr → Prop
    | const : Π {v}, is_value (expr.const v)
    -- TODO: arrays


  -- ENVIRONMENTS

  inductive env : Type
    | empty : env
    | cons_expr : string → type → env → env
    | cons_type : string → kind → env → env

  instance : has_emptyc env :=
    ⟨env.empty⟩

  instance env_insert_expr : has_insert (string × type) env :=
    ⟨λ binding, env.cons_expr binding.1 binding.2⟩

  instance env_insert_type : has_insert (string × kind) env :=
    ⟨λ binding, env.cons_type binding.1 binding.2⟩

  protected def env.mem_expr (x₁ : string) (τ₁ : type) : env → Prop
    | env.empty := false
    | (env.cons_expr x₂ τ₂ Γ) := if x₁ = x₂ then τ₁ = τ₂ else env.mem_expr Γ
    | (env.cons_type x₂ _ Γ) := if x₁ = x₂ then false else env.mem_expr Γ

  protected def env.mem_type (x₁ : string) (κ₁ : kind) : env → Prop
    | env.empty := false
    | (env.cons_expr x₂ _ Γ) := if x₁ = x₂ then false else env.mem_type Γ
    | (env.cons_type x₂ κ₂ Γ) := if x₁ = x₂ then κ₁ = κ₂ else env.mem_type Γ

  instance env_mem_expr : has_mem (string × type) env :=
    ⟨λ binding, env.mem_expr binding.1 binding.2⟩

  instance env_mem_type : has_mem (string × kind) env :=
    ⟨λ binding, env.mem_type binding.1 binding.2⟩

  protected def env.lookup_expr (x₁ : string) : env → option type
    | env.empty := none
    | (env.cons_expr x₂ τ₂ Γ) := if x₁ = x₂ then some τ₂ else env.lookup_expr Γ
    | (env.cons_type x₂ _ Γ) := if x₁ = x₂ then none else env.lookup_expr Γ

  protected def env.lookup_type (x₁ : string) : env → option kind
    | env.empty := none
    | (env.cons_expr x₂ _ Γ) := if x₁ = x₂ then none else env.lookup_type Γ
    | (env.cons_type x₂ κ₂ Γ) := if x₁ = x₂ then some κ₂ else env.lookup_type Γ


  -- EVALUATION RULES

  def relation (X : Type) :=
    X → X → Prop

  inductive multi {X : Type} (R : relation X) : relation X
    | refl : Π {x : X}, multi x x
    | step : Π {x y z : X}, R x y → multi y z → multi x z.

  reserve infixl ` ⟹ `:50
  reserve infixl ` ⟹*`:50

  inductive step : expr → expr → Prop
    infixl ` ⟹ ` := step

    | const : Π {c},
        expr.const c ⟹ expr.const c

    | op_rec_l : Π {op e₁ e₁' e₂},
        e₁ ⟹ e₁' →
        expr.app_op op e₁ e₂ ⟹ expr.app_op op e₁' e₂

    | op_rec_r : Π {op e₁ e₂ e₂'},
        is_value e₁ →
        e₂ ⟹ e₂' →
        expr.app_op op e₁ e₂ ⟹ expr.app_op op e₁ e₂'

    | op_add : Π {n m},
        expr.const (value.nat n) + expr.const (value.nat m) ⟹
            expr.const (value.nat (n + m))

    | op_mul : Π {n m},
        expr.const (value.nat n) * expr.const (value.nat m) ⟹
            expr.const (value.nat (n * m))

    -- FIXME: Lookup context for var name?
    | var : Π {x},
        expr.var x ⟹ sorry

    | proj : Π {x e e'},
        e ⟹ e' →
        expr.proj e' x ⟹ sorry

  infixl ` ⟹ ` := step
  infixl ` ⟹* ` := multi step


  -- TYPING RULES

  inductive has_type : env → expr → type → Prop
    notation `τ[ ` Γ ` ⊢ ` e ` : ` τ ` ]` := has_type Γ e τ

    | bool : Π {Γ b},
        τ[ Γ ⊢ expr.const (value.bool b) : type.bool ]

    | nat : Π {Γ n},
        τ[ Γ ⊢ expr.const (value.nat n) : type.nat ]

    | add : Π {Γ e₁ e₂},
        τ[ Γ ⊢ e₁ : type.nat ] →
        τ[ Γ ⊢ e₂ : type.nat ] →
        τ[ Γ ⊢ e₁ + e₂ : type.nat ]

    | mul : Π {Γ e₁ e₂},
        τ[ Γ ⊢ e₁ : type.nat ] →
        τ[ Γ ⊢ e₂ : type.nat ] →
        τ[ Γ ⊢ e₁ * e₂ : type.nat ]

    | var : Π {Γ x τ},
        (x, τ) ∈ Γ →
        τ[ Γ ⊢ expr.var x : τ ]

    | proj : Π {Γ e x τ₁ τ₂},
        τ[ Γ ⊢ e : τ₁ ] → -- FIXME: τ₁ : struct
        τ[ Γ ⊢ expr.proj e x : τ₂ ]

    | index : Π {Γ a i e τ},
        τ[ Γ ⊢ a : type.array τ e ] →
        τ[ Γ ⊢ i : type.nat ] →
        τ[ Γ ⊢ expr.index a i : τ ]

  notation `τ[ ` Γ ` ⊢ ` e ` : ` τ ` ]` := has_type Γ e τ


  inductive has_kind : env → type → kind → Prop
    notation `κ[ ` Γ ` ⊢ ` τ ` : ` κ ` ]` := has_kind Γ τ κ

    | bool : Π {Γ},
        κ[ Γ ⊢ type.bool : kind.host ]

    | nat : Π {Γ},
        κ[ Γ ⊢ type.nat : kind.host ]

    | u8 : Π {Γ},
        κ[ Γ ⊢ type.u8 : kind.binary ]

    | var : Π {Γ x κ},
        (x, κ) ∈ Γ →
        κ[ Γ ⊢ type.var x : κ ]

    | abs : Π {Γ x τ₁ κ₁ κ₂},
        κ[ (insert (x, κ₁) Γ) ⊢ τ₁ : κ₂ ] →
        κ[ Γ ⊢ Λ x : κ₁, τ₁ : (κ₁ ↣ κ₂) ]

    | app : Π {Γ τ₁ τ₂ κ₁ κ₂},
        κ[ Γ ⊢ τ₁ : (κ₁ ↣ κ₂) ] →
        κ[ Γ ⊢ τ₂ : κ₁ ] →
        κ[ Γ ⊢ type.app τ₁ τ₂ : κ₂ ]

    -- arrays take on the kind of their elements
    -- size expressions must always evaluate to natural numbers
    | array : Π {Γ τ κ e},
        κ[ Γ ⊢ τ : κ ] →
        τ[ Γ ⊢ e : type.nat ] →
        κ[ Γ ⊢ type.array τ e : κ ]

    | sum : Π {Γ τ₁ τ₂},
        κ[ Γ ⊢ τ₁ : kind.binary ] →
        κ[ Γ ⊢ τ₂ : kind.binary ] →
        κ[ Γ ⊢ type.sum τ₁ τ₂ : kind.binary ]

    -- structs are always binary, and subsequent fields can
    -- access previous fields
    | struct : Π {Γ x τ₁ τ₂},
        κ[ Γ ⊢ τ₁ : kind.binary ] → -- FIXME: τ₁
        κ[ insert (x, kind.binary) Γ ⊢ τ₂ : kind.binary ] → -- FIXME: should be `insert (x, τ₁)`?
        κ[ Γ ⊢ type.struct x τ₁ τ₂ : kind.binary ]

  notation `κ[ ` Γ ` ⊢ ` τ ` : ` κ ` ]` := has_kind Γ τ κ


  section
    open expr type kind

    example : κ[ {("x", binary)} ⊢  array (var "x") (expr.const (value.nat 1)) : binary ] :=
      has_kind.array (has_kind.var rfl) has_type.nat

    example : κ[ ∅ ⊢  Λ "x" : binary, array (var "x") (expr.const (value.nat 1) + expr.const (value.nat 2)) : (binary ↣ binary) ] :=
      has_kind.abs (has_kind.array (has_kind.var rfl)
                   (has_type.add has_type.nat has_type.nat))
  end


  -- PROGRESS
  -- https://softwarefoundations.cis.upenn.edu/plf-current/StlcProp.html#lab220

  theorem progress (e : expr) (τ : type) :
    -- FIXME: Kinding?
    τ[ ∅ ⊢ e : τ ] →
    is_value e ∨ ∃ e', e ⟹ e' :=
      assume h,
        sorry


  -- PRESERVATION
  -- https://softwarefoundations.cis.upenn.edu/plf-current/StlcProp.html#lab222

  theorem preservation (e e' : expr) (τ : type) :
    -- FIXME: Kinding?
    τ[ ∅ ⊢ e : τ ] →
    e ⟹ e' →
    τ[ ∅ ⊢ e' : τ ] :=
      assume hτ hstep,
        sorry


  -- TYPE CHECKING
  -- https://softwarefoundations.cis.upenn.edu/plf-current/Typechecking.html#lab333

  def type_check (Γ : env) : expr → option type
    | (expr.const (value.bool _)) := some type.bool
    | (expr.const (value.nat _)) := some type.nat
    | (expr.app_op op.add e₁ e₂) :=
        match type_check e₁, type_check e₂ with
          | some type.nat, some type.nat := some type.nat
          | _, _ := none
        end
    | (expr.app_op op.mul e₁ e₂) :=
        match type_check e₁, type_check e₂ with
          | some type.nat, some type.nat := some type.nat
          | _, _ := none
        end
    | (expr.var x) := env.lookup_expr x Γ
    | (expr.proj _ _) := sorry
    | (expr.index _ _) := sorry

  theorem type_checking_sound {Γ e τ} :
    type_check Γ e = some τ →
    τ[ Γ ⊢ e : τ ] :=
      assume htc,
        sorry

end ddl

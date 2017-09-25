namespace ddl

  -- SYNTAX

  inductive endianness : Type
    | little
    | big

  mutual inductive expr, type, kind
    with expr : Type
      | true : expr
      | false : expr
      | nat : ℕ → expr
      | add : expr → expr → expr
      | mul : expr → expr → expr
      | var : string → expr
      | interp : expr → expr

    with type : Type
      | bool : type
      | nat : type
      | u8 : type
      | u16 : endianness → type
      | var : string → type
      | abs : string → kind → type → type
      | app : type → type → type
      | struct : string → type → type → type
      | array : type → expr → type

    with kind : Type
      | host : kind
      | binary : kind
      | arrow : kind → kind → kind

  infix + := expr.add
  infix * := expr.mul
  notation `Λ` x `:` κ `,` τ := type.abs x κ τ
  notation κ₁ ` ↣ ` κ₂ := kind.arrow κ₁ κ₂


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

  protected def env.mem_expr : string → type → env → Prop
    | _ _ env.empty := false
    | x₁ τ₁ (env.cons_expr x₂ τ₂ Γ) := if x₁ = x₂ then τ₁ = τ₂ else env.mem_expr x₁ τ₁ Γ
    | x₁ τ₁ (env.cons_type x₂ _ Γ) := if x₁ = x₂ then false else env.mem_expr x₁ τ₁ Γ

  protected def env.mem_type : string → kind → env → Prop
    | _ _ env.empty := false
    | x₁ κ₁ (env.cons_expr x₂ _ Γ) := if x₁ = x₂ then false else env.mem_type x₁ κ₁ Γ
    | x₁ κ₁ (env.cons_type x₂ κ₂ Γ) := if x₁ = x₂ then κ₁ = κ₂ else env.mem_type x₁ κ₁ Γ

  instance env_mem_expr : has_mem (string × type) env :=
    ⟨λ binding, env.mem_expr binding.1 binding.2⟩

  instance env_mem_type : has_mem (string × kind) env :=
    ⟨λ binding, env.mem_type binding.1 binding.2⟩


  -- EVALUATION

  def relation (X : Type) :=
    X → X → Prop

  inductive multi {X : Type} (R : relation X) : relation X
    | refl : Π {x : X}, multi x x
    | step : Π {x y z : X}, R x y → multi y z → multi x z.

  reserve infixl ` ⟹ `:50
  reserve infixl ` ⟹*`:50

  inductive step : expr → expr → Prop
    infixl ` ⟹ ` := step

    | true :
        expr.true ⟹ expr.true

    | false :
        expr.false ⟹ expr.false

    | nat : Π {n},
        expr.nat n ⟹ expr.nat n

    | add : Π {n m},
        expr.nat n + expr.nat m ⟹ expr.nat (n + m)

    | mul : Π {n m},
        expr.nat n * expr.nat m ⟹ expr.nat (n * m)

    | var : Π {x},
        expr.var x ⟹ sorry

    | interp : Π {e},
        expr.interp e ⟹ sorry

  infixl ` ⟹ ` := step
  infixl ` ⟹* ` := multi step


  -- TYPING

  inductive type_of : env → expr → type → Prop
    notation `τ[ ` Γ ` ⊢ ` e ` : ` τ ` ]` := type_of Γ e τ

    | true : Π {Γ},
        τ[ Γ ⊢ expr.true : type.bool ]

    | false : Π {Γ},
        τ[ Γ ⊢ expr.false : type.bool ]

    | nat : Π {Γ n},
        τ[ Γ ⊢ expr.nat n : type.nat ]

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

    | interp_nat_to_u8 : Π {Γ e},
        τ[ Γ ⊢ e : type.nat ] →
        τ[ Γ ⊢ expr.interp e : type.u8 ]

    | interp_nat_to_u16 : Π {Γ e E},
        τ[ Γ ⊢ e : type.nat ] →
        τ[ Γ ⊢ expr.interp e : type.u16 E ]

    | interp_u8_to_nat : Π {Γ e},
        τ[ Γ ⊢ e : type.u8 ] →
        τ[ Γ ⊢ expr.interp e : type.nat ]

    | interp_u16_to_nat : Π {Γ e E},
        τ[ Γ ⊢ e : type.u16 E ] →
        τ[ Γ ⊢ expr.interp e : type.nat ]

  notation `τ[ ` Γ ` ⊢ ` e ` : ` τ ` ]` := type_of Γ e τ


  -- KINDING

  inductive kind_of : env → type → kind → Prop
    notation `κ[ ` Γ ` ⊢ ` τ ` : ` κ ` ]` := kind_of Γ τ κ

    | bool : Π {Γ},
        κ[ Γ ⊢ type.bool : kind.host ]

    | nat : Π {Γ},
        κ[ Γ ⊢ type.nat : kind.host ]

    | u8 : Π {Γ},
        κ[ Γ ⊢ type.u8 : kind.binary ]

    | u16 : Π {Γ E},
        κ[ Γ ⊢ type.u16 E : kind.binary ]

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

    -- structs are always binary, and subsequent fields can
    -- access previous fields
    | struct : Π {Γ x τ₁ τ₂},
        κ[ Γ ⊢ τ₁ : kind.binary ] →
        κ[ insert (x, kind.binary) Γ ⊢ τ₂ : kind.binary ] →
        κ[ Γ ⊢ type.struct x τ₁ τ₂ : kind.binary ]

  notation `κ[ ` Γ ` ⊢ ` τ ` : ` κ ` ]` := kind_of Γ τ κ


  section
    open endianness expr type kind

    example : κ[ {("x", binary)} ⊢  array (var "x") (expr.nat 1) : binary ] :=
      kind_of.array (kind_of.var rfl) type_of.nat

    example : κ[ ∅ ⊢  Λ "x" : binary, array (var "x") (expr.nat 1 + expr.nat 2) : (binary ↣ binary) ] :=
      kind_of.abs (kind_of.array (kind_of.var rfl)
                  (type_of.add type_of.nat type_of.nat))
  end

  -- PROOFS

  -- TODO

end ddl

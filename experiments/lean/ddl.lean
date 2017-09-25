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
    | cons : string × kind → env → env

  instance : has_emptyc env :=
    ⟨env.empty⟩

  instance : has_insert (string × kind) env :=
    ⟨env.cons⟩

  def env.mem : string × kind → env → Prop
    | (_, _) env.empty :=
        false
    | (x₁, b₁) (env.cons (x₂, b₂) Γ) :=
      if x₁ = x₂ then b₁ = b₂ else env.mem (x₁, b₁) Γ

  instance : has_mem (string × kind) env :=
    ⟨env.mem⟩


  -- RULES

  inductive type_of : env → expr → type → Type
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

  inductive kind_of : env → type → kind → Type
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

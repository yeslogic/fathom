namespace ddl

  -- SYNTAX

  inductive op : Type
    | add
    | mul

  inductive value : Type
    | bool : bool → value
    | nat : ℕ → value

  inductive expr : Type
    | const : value → expr
    | app_op : op → expr → expr → expr
    | var : string → expr
    | proj : expr → string → expr
    | index : expr → expr → expr
    | cond : expr → expr → expr → expr

  mutual inductive type, kind
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
      | cond : expr → type → type → type
      | interp : string → type → expr → type

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
    | refl {x : X} : multi x x
    | step {x y z : X} : R x y → multi y z → multi x z.

  reserve infixl ` ⟹ `:50
  reserve infixl ` ⟹*`:50

  inductive step : expr → expr → Prop
    infixl ` ⟹ ` := step

    | const {c} :
        expr.const c ⟹ expr.const c

    | op_rec_l {op e₁ e₁' e₂} :
        e₁ ⟹ e₁' →
        expr.app_op op e₁ e₂ ⟹ expr.app_op op e₁' e₂

    | op_rec_r {op e₁ e₂ e₂'} :
        is_value e₁ →
        e₂ ⟹ e₂' →
        expr.app_op op e₁ e₂ ⟹ expr.app_op op e₁ e₂'

    | op_add {n m} :
        expr.const (value.nat n) + expr.const (value.nat m) ⟹
            expr.const (value.nat (n + m))

    | op_mul {n m} :
        expr.const (value.nat n) * expr.const (value.nat m) ⟹
            expr.const (value.nat (n * m))

    -- FIXME: Lookup context for var name?
    | var {x} :
        expr.var x ⟹ sorry

    | proj {x e e'} :
        e ⟹ e' →
        expr.proj e' x ⟹ sorry

    | cond_rec {c₁ c₂ e₁ e₂} :
        c₁ ⟹ c₂ →
        expr.cond c₁ e₁ e₂ ⟹ expr.cond c₂ e₁ e₂

    | cond_true {e₁ e₂} :
        expr.cond (expr.const (value.bool true)) e₁ e₂ ⟹ e₁

    | cond_false {e₁ e₂} :
        expr.cond (expr.const (value.bool false)) e₁ e₂ ⟹ e₂


  infixl ` ⟹ ` := step
  infixl ` ⟹* ` := multi step

  inductive type_eval : env → type → type → Prop

    | var {Γ x τ} :
        (x, τ) ∈ Γ →
        type_eval Γ (type.var x) τ

    | cond_rec {Γ c₁ c₂ τ₁ τ₂} :
        c₁ ⟹ c₂ →
        type_eval Γ (type.cond c₁ τ₁ τ₂) (type.cond c₂ τ₁ τ₂)

    | cond_true {Γ τ₁ τ₂} :
        type_eval Γ (type.cond (expr.const (value.bool true)) τ₁ τ₂) τ₁

    | cond_false {Γ τ₁ τ₂} :
        type_eval Γ (type.cond (expr.const (value.bool false)) τ₁ τ₂) τ₂


  -- TYPING RULES

  inductive has_type : env → expr → type → Prop
    notation `τ[ ` Γ ` ⊢ ` e ` : ` τ ` ]` := has_type Γ e τ

    | bool {Γ b} :
        τ[ Γ ⊢ expr.const (value.bool b) : type.bool ]

    | nat {Γ n} :
        τ[ Γ ⊢ expr.const (value.nat n) : type.nat ]

    | add {Γ e₁ e₂} :
        τ[ Γ ⊢ e₁ : type.nat ] →
        τ[ Γ ⊢ e₂ : type.nat ] →
        τ[ Γ ⊢ e₁ + e₂ : type.nat ]

    | mul {Γ e₁ e₂} :
        τ[ Γ ⊢ e₁ : type.nat ] →
        τ[ Γ ⊢ e₂ : type.nat ] →
        τ[ Γ ⊢ e₁ * e₂ : type.nat ]

    | var {Γ x τ} :
        (x, τ) ∈ Γ →
        τ[ Γ ⊢ expr.var x : τ ]

    | proj {Γ e x τ₁ τ₂} :
        τ[ Γ ⊢ e : τ₁ ] → -- FIXME: τ₁ : struct
        τ[ Γ ⊢ expr.proj e x : τ₂ ]

    | index {Γ a i e τ} :
        τ[ Γ ⊢ a : type.array τ e ] →
        τ[ Γ ⊢ i : type.nat ] →
        τ[ Γ ⊢ expr.index a i : τ ]

    | cond {Γ e₁ e₂ e₃ τ} :
        τ[ Γ ⊢ e₁ : type.bool ] →
        τ[ Γ ⊢ e₂ : τ ] →
        τ[ Γ ⊢ e₃ : τ ] →
        τ[ Γ ⊢ expr.cond e₁ e₂ e₃ : τ ]

  notation `τ[ ` Γ ` ⊢ ` e ` : ` τ ` ]` := has_type Γ e τ


  inductive has_kind : env → type → kind → Prop
    notation `κ[ ` Γ ` ⊢ ` τ ` : ` κ ` ]` := has_kind Γ τ κ

    | bool {Γ} :
        κ[ Γ ⊢ type.bool : kind.host ]

    | nat {Γ} :
        κ[ Γ ⊢ type.nat : kind.host ]

    | u8 {Γ} :
        κ[ Γ ⊢ type.u8 : kind.binary ]

    | var {Γ x κ} :
        (x, κ) ∈ Γ →
        κ[ Γ ⊢ type.var x : κ ]

    | abs {Γ x τ₁ κ₁ κ₂} :
        κ[ (insert (x, κ₁) Γ) ⊢ τ₁ : κ₂ ] →
        κ[ Γ ⊢ Λ x : κ₁, τ₁ : (κ₁ ↣ κ₂) ]

    | app {Γ τ₁ τ₂ κ₁ κ₂} :
        κ[ Γ ⊢ τ₁ : (κ₁ ↣ κ₂) ] →
        κ[ Γ ⊢ τ₂ : κ₁ ] →
        κ[ Γ ⊢ type.app τ₁ τ₂ : κ₂ ]

    -- arrays take on the kind of their elements
    -- size expressions must always evaluate to natural numbers
    | array {Γ τ κ e} :
        κ[ Γ ⊢ τ : κ ] →
        τ[ Γ ⊢ e : type.nat ] →
        κ[ Γ ⊢ type.array τ e : κ ]

    | sum {Γ τ₁ τ₂} :
        κ[ Γ ⊢ τ₁ : kind.binary ] →
        κ[ Γ ⊢ τ₂ : kind.binary ] →
        κ[ Γ ⊢ type.sum τ₁ τ₂ : kind.binary ]

    -- structs are always binary, and subsequent fields can
    -- access previous fields
    | struct {Γ x τ₁ τ₂} :
        κ[ Γ ⊢ τ₁ : kind.binary ] → -- FIXME: τ₁
        κ[ insert (x, kind.binary) Γ ⊢ τ₂ : kind.binary ] → -- FIXME: should be `insert (x, τ₁)`?
        κ[ Γ ⊢ type.struct x τ₁ τ₂ : kind.binary ]

    -- condition branches can have different types, but both
    -- types must have the same kind
    | cond {Γ e τ₁ τ₂ κ} :
        τ[ Γ ⊢ e : type.bool ] →
        κ[ Γ ⊢ τ₁ : κ ] →
        κ[ Γ ⊢ τ₂ : κ ] →
        κ[ Γ ⊢ type.cond e τ₁ τ₂ : κ ]

    -- should it still have binary kind if it's interpreting a binary type?
    | interp : Π {Γ x e τ₁ κ},
        κ[ Γ ⊢ τ₁ : κ ] →
        κ[ Γ ⊢ type.interp x τ₁ e : κ ]

  inductive has_rep : env → type → type → Prop

    | u8 {Γ} :
        has_rep Γ type.u8 type.nat

    | var {Γ x τ₁ τ₂} :
        (x, τ₁) ∈ Γ →
        has_rep Γ τ₁ τ₂ →
        has_rep Γ (type.var x) τ₂

    | array {Γ τ₁ τ₂ e} :
        has_rep Γ τ₁ τ₂ →
        has_rep Γ (type.array τ₁ e) (type.array τ₂ e)

    | cond {Γ e τ₁ τ₂ r₁ r₂} :
        has_rep Γ τ₁ r₁ →
        has_rep Γ τ₂ r₂ →
        has_rep Γ (type.cond e τ₁ τ₂) (type.cond e r₁ r₂)

    | interp {Γ x e τ₁ τ₂ τ₃} :
        has_rep Γ τ₁ τ₂ →
        τ[ (insert (x, τ₂) Γ) ⊢ e : τ₃ ] →
        has_rep Γ (type.interp x τ₁ e) τ₃

  notation `κ[ ` Γ ` ⊢ ` τ ` : ` κ ` ]` := has_kind Γ τ κ

  -- MATCHING

  inductive byte : Type
    | byte {n:nat} : n < 256 → byte

  inductive matches : type → list byte → Prop

    | u8 {b} : matches type.u8 (b :: list.nil)

    | struct {x τ₁ τ₂ bs₁ bs₂} :
        matches τ₁ bs₁ →
        matches τ₂ bs₂ →
        matches (type.struct x τ₁ τ₂) (bs₁ ++ bs₂)

    | array_nil {τ} :
        matches (type.array τ (expr.const (value.nat 0))) list.nil

    | array_cons {τ n bs₁ bs₂} :
        matches τ bs₁ →
        matches (type.array τ (expr.const (value.nat n))) bs₂ →
        matches (type.array τ (expr.const (value.nat (nat.succ n)))) (bs₁ ++ bs₂)

  def match_repeat (T:Type) (f:list T → option (list T × list T)) : nat → list T → option (list T × list T)
    | 0 bs := some (list.nil, bs)
    | (nat.succ n) bs :=
        match f bs with
        | some (bs', bs'') :=
            match match_repeat n bs'' with
            | some (bs''', bs'''') := some (bs' ++ bs''', bs'''')
            | none := none
            end
        | none := none
        end

  def match_type : type → list byte → option (list byte × list byte)
    | (type.u8) (b :: bs) := some (b::list.nil, bs)
    | (type.struct _ τ₁ τ₂) bs :=
        match match_type τ₁ bs with
        | some (bs', bs'') :=
            match match_type τ₂ bs'' with
            | some (bs''', bs'''') := some (bs' ++ bs''', bs'''')
            | none := none
            end
        | none := none
        end
    | (type.array τ (expr.const (value.nat n))) bs :=
        match_repeat byte (match_type τ) n bs
    | _ _ := none

  theorem match_type_suffix {τ bs bs'} :
    match_type τ bs = some (bs, list.nil) →
    match_type τ (bs ++ bs') = some (bs, bs') :=
    sorry.

  theorem match_type_correct {τ bs} : matches τ bs → match_type τ bs = some (bs, list.nil) :=
  begin
    intro h,
    induction h,
    {
        refl
    },
    {
        unfold match_type,
        have h3 : match_type τ₁ (bs₁ ++ bs₂) = some (bs₁, bs₂),
        {
            apply match_type_suffix,
            apply ih_1
        },
        rewrite h3,
        unfold match_type._match_1,
        rewrite ih_2,
        unfold match_type._match_2
    },
    {
        unfold match_type,
        unfold match_repeat
    },
    {
        unfold match_type,
        unfold match_type at ih_2,
        unfold match_repeat,
        have h3 : match_type τ_1 (bs₁ ++ bs₂) = some (bs₁, bs₂),
        {
            apply match_type_suffix,
            apply ih_1
        },
        rewrite h3,
        unfold match_repeat._match_1,
        rewrite ih_2,
        unfold match_repeat._match_2
    }
  end

  theorem match_type_correct {τ bs bs' bs''} : match_type τ bs = some (bs', bs'') → matches τ bs' :=
  begin
    induction τ,
    {
        unfold match_type,
        intro h,
        cases h
    },
    {
        unfold match_type,
        intro h,
        cases h
    },
    {
        cases bs,
        {
            unfold match_type,
            intro h,
            cases h
        },
        {
            unfold match_type,
            intro h,
            cases h,
            constructor
        }
    },
    {
        unfold match_type,
        intro h,
        cases h
    },
    {
        unfold match_type,
        intro h,
        cases h
    },
    {
        unfold match_type,
        intro h,
        cases h
    },
    {
        unfold match_type,
        intro h,
        cases h
    },
    {
        unfold match_type,
        intro h,
        have h1 : bs = bs' ++ bs'',
        {

        },
        {

        }
        apply matches.struct,
        sorry
    }
    end

  -- EXPERIMENT

  inductive valueT : type → Type
    | boolT : bool → valueT type.bool
    | natT : ℕ → valueT type.nat

  inductive exprT : type → Type
    | constT {τ} : valueT τ → exprT τ
    | app_opT : op → exprT type.nat → exprT type.nat → exprT type.nat
    | varT {Γ:env} {x:string} {τ} : (x, τ) ∈ Γ → exprT τ
    --| projT : expr → string → expr
    | indexT {e τ} : exprT (type.array τ e) → exprT type.nat → exprT τ
    | condT {τ} : exprT type.bool → exprT τ → exprT τ → exprT τ



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
    | (expr.cond e₁ e₂ e₃) :=
        match type_check e₁, type_check e₂, type_check e₃ with
          | some type.bool, some τ₁, some τ₂ := some (type.cond e₁ τ₁ τ₂)
          | _, _, _ := none
        end

  theorem type_checking_sound {Γ e τ} :
    type_check Γ e = some τ →
    τ[ Γ ⊢ e : τ ] :=
      assume htc,
        sorry

end ddl

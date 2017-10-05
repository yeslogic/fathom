/- An attempt to formalise the DDL by using two ASTs - one for host types, and
   one for binary types.
-/

import data.vector

namespace ddl

  def relation (α : Type) :=
    α → α → Prop

  inductive multi {α : Type} (ρ : relation α) : relation α
    | refl {x : α} : multi x x
    | step {x y z : α} : ρ x y → multi y z → multi x z.

  /- The host language -/
  namespace host

    -- SYNTAX

    /- The type syntax of the host language -/
    inductive type : Type
      | bool : type
      | nat : type


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
    instance : has_add expr := ⟨expr.app_binop binop.add⟩
    instance : has_mul expr := ⟨expr.app_binop binop.mul⟩


    /- 'Stuck' values -/
    inductive expr.value : expr → Prop
      | bool {bv} : expr.value (expr.bool bv)
      | nat {nv} : expr.value (expr.nat nv)


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
      | type.bool := bool
      | type.nat := ℕ


    def typed_expr.embed : Π (e : typed_expr), type.embed e.t
      | ⟨expr.bool b, type.bool, h⟩ := b
      | ⟨expr.nat n,  type.nat,  h⟩ := n
      | ⟨e₁ + e₂,     type.nat,  h⟩ := sorry
      -- | ⟨e₁ * e₂,     type.nat,  h⟩ := sorry
      | ⟨_,           _,         _⟩ := sorry -- hmmm...


    -- EVALUATION RULES

    reserve infixl ` ⟹ `:50
    reserve infixl ` ⟹* `:50

    inductive step : expr → expr → Prop
      infixl ` ⟹ ` := step

      | value {e} :
          expr.value e →
          e ⟹ e
      | binop_rec_l {op e₁ e₁' e₂} :
          e₁ ⟹ e₁' →
          expr.app_binop op e₁ e₂ ⟹ expr.app_binop op e₁' e₂
      | binop_rec_r {op e₁ e₂ e₂'} :
          expr.value e₁ →
          e₂ ⟹ e₂' →
          expr.app_binop op e₁ e₂ ⟹ expr.app_binop op e₁ e₂'
      | binop_add {nv₁ nv₂} :
          expr.nat nv₁ + expr.nat nv₂ ⟹ expr.nat (nv₁ + nv₂)
      | binop_mul {nv₁ nv₂} :
          expr.nat nv₁ * expr.nat nv₂ ⟹ expr.nat (nv₁ * nv₂)

    infixl ` ⟹ ` := step
    infixl ` ⟹* ` := multi step


    -- PROGRESS
    -- https://softwarefoundations.cis.upenn.edu/plf-current/StlcProp.html#lab220

    theorem progress (e : expr) (t : type) :
      has_type e t →
      expr.value e ∨ ∃ e', e ⟹ e' :=
    begin
      intro ht,
      induction ht,
      case has_type.bool {
        exact sorry
      },
      case has_type.nat {
        exact sorry
      },
      case has_type.add {
        exact sorry
      },
      case has_type.mul {
        exact sorry
      },
    end


    -- PRESERVATION
    -- https://softwarefoundations.cis.upenn.edu/plf-current/StlcProp.html#lab222

    theorem preservation (e e' : expr) (t : type) :
      has_type e t →
      e ⟹ e' →
      has_type e' t :=
    begin
      exact sorry
    end

  end host

  /- The binary language -/
  namespace binary

    -- SYNTAX

    /- Kinds of types in the binary language -/
    inductive kind : Type
      | type : kind
      | arrow : kind → kind → kind

    notation `★` := kind.type
    notation k₁ ` ⇒ ` k₂ := kind.arrow k₁ k₂


    /- The type system of the binary language -/
    inductive type : Type
      | var : ℕ → type
      | unit : type
      | bit : type
      | sum : type → type → type
      | prod : type → type → type
      | array : type → host.expr → type
      | cond : type → host.expr → type
      | abs : kind → type → type
      | app : type → type → type

    -- Type variables
    prefix `#` := type.var
    -- Overload the `+` operator for constructing sum types
    instance : has_add type := ⟨type.sum⟩
    -- Product, abstraction and conditional type notation - note that we are a
    -- using nameless for identifiers encoding so we don't include the argument
    -- identifiers
    notation `Σ0: ` t₁ `, ` t₂ := type.prod t₁ t₂
    notation `Λ0: ` k `, ` t := type.abs k t
    notation `{0: ` t ` | ` e ` }` := type.cond t e
    -- Array type syntax
    notation `[ ` t `; ` e ` ]` := type.array t e
    -- Application operator
    infixl ` ∙ `:50 := type.app


    -- CONTEXTS

    def ctx : Type :=
      list kind

    def ctx.lookup (n : ℕ) (Γ : ctx) : n < Γ.length → kind :=
      assume is_lt,
        list.nth_le Γ n is_lt


    -- KINDING RULES

    inductive has_kind : ctx → type → kind → Prop
      | var {Γ} (x) {is_lt} :
          has_kind Γ #x (ctx.lookup x Γ is_lt)
      | unit {Γ} :
          has_kind Γ type.unit ★
      | bit {Γ} :
          has_kind Γ type.bit ★
      | sum {Γ t₁ t₂} :
          has_kind Γ t₁ ★ →
          has_kind Γ t₂ ★ →
          has_kind Γ (t₁ + t₂) ★
      | prod {Γ t₁ t₂} :
          has_kind Γ t₁ ★ →
          has_kind Γ t₂ ★ →
          has_kind Γ (Σ0: t₁, t₂) ★
      | array {Γ t e} :
          has_kind Γ t ★ →
          host.has_type e host.type.nat →
          has_kind Γ [ t; e ] ★
      | cond {Γ t e} :
          has_kind Γ t ★ →
          host.has_type e host.type.bool →
          has_kind Γ {0: t | e } ★
      | abs {Γ t k₁ k₂} :
          has_kind (k₁ :: Γ) t k₁ →
          has_kind Γ (Λ0: k₁, t) (k₁ ⇒ k₂)
      | app {Γ t₁ t₂ k₁ k₂} :
          has_kind Γ t₁ (k₁ ⇒ k₂) →
          has_kind Γ t₂ k₁ →
          has_kind Γ (t₁ ∙ t₂) k₂


    -- EMBEDDING

    /- Embed a kind as a Lean term -/
    def kind.embed : kind → Type 1
      | kind.type := Type 0
      | (kind.arrow k₁ k₂) := kind.embed k₁ → kind.embed k₂

  end binary

end ddl

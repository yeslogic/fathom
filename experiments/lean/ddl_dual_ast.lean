/- An attempt to formalise the DDL by using two ASTs - one for host types, and
   one for binary types.
-/

import data.vector

namespace ddl

  /- The host language -/
  namespace host

    /- The type syntax of the host language -/
    inductive type : Type
      | bool : type
      | nat : type


    /- embed a host type into Lean -/
    @[reducible]
    def type.embed : type → Type
      | type.bool := bool
      | type.nat := ℕ


    /- The expression syntax of the host language -/
    inductive expr : Type
      | bool : bool → expr
      | nat : ℕ → expr
      | add : expr → expr → expr
      | mul : expr → expr → expr

    instance has_coe_to_bool : has_coe bool expr := ⟨expr.bool⟩
    instance has_coe_to_nat : has_coe ℕ expr  := ⟨expr.nat⟩
    instance : has_add expr := ⟨expr.add⟩
    instance : has_mul expr := ⟨expr.add⟩


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


    structure typed_expr : Type :=
      (e : expr)
      (t : type)
      (h : has_type e t)

    def typed_expr.embed : Π (e : typed_expr), type.embed e.t
      | ⟨expr.bool b,    type.bool, h⟩ := b
      | ⟨expr.nat n,     type.nat,  h⟩ := n
      | ⟨expr.add e₁ e₂, type.nat,  h⟩ := sorry
      | ⟨expr.mul e₁ e₂, type.nat,  h⟩ := sorry
      | ⟨_,                _,       _⟩ := sorry -- hmmm...

  end host

  /- The binary language -/
  namespace binary

    /- Kinds of types in the binary language

       These will let us express quantification and application at the type
       level.
    -/
    inductive kind : Type
      | type : kind
      | arrow : kind → kind → kind

    notation `★` := kind.type
    notation k₁ ` ⇒ ` k₂ := kind.arrow k₁ k₂


    /- Embed a kind as a Lean term -/
    def kind.embed : kind → Type 1
      | kind.type := Type 0
      | (kind.arrow k₁ k₂) := kind.embed k₁ → kind.embed k₂


    /- The type system of the binary language -/
    inductive type : Type
      | var   : ℕ → type
      | unit  : type
      | bit   : type
      | sum   : type → type → type
      | prod  : type → type → type
      | array : type → host.expr → type
      | abs   : kind → type → type
      | app   : type → type → type

    notation `Λ ` k `, ` t := type.abs k t
    prefix `#` := type.var
    infixl ` ∙ `:50 := type.app

    instance : has_add type := ⟨type.sum⟩
    instance : has_mul type := ⟨type.prod⟩


    def ctx : Type :=
      list kind

    def ctx.lookup (n : ℕ) (Γ : ctx) : n < Γ.length → kind :=
      assume is_lt,
        list.nth_le Γ n is_lt


    inductive has_kind : ctx → type → kind → Type
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
          has_kind Γ (t₁ * t₂) ★
      | array {Γ t e} :
          has_kind Γ t ★ →
          host.has_type e host.type.nat →
          has_kind Γ (type.array t e) ★
      | abs {Γ t k₁ k₂} :
          has_kind (k₁ :: Γ) t k₁ →
          has_kind Γ (Λ k₁, t) (k₁ ⇒ k₂)
      | app {Γ t₁ t₂ k₁ k₂} :
          has_kind Γ t₁ (k₁ ⇒ k₂) →
          has_kind Γ t₂ k₁ →
          has_kind Γ (t₁ ∙ t₂) k₂

  end binary

end ddl

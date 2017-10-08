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
      | bool (bv : bool) : expr.value (expr.bool bv)
      | nat (nv : ℕ) : expr.value (expr.nat nv)


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

    theorem progress :
      Π (e : expr) (t : type),
      has_type e t →
      expr.value e ∨ ∃ e', e ⟹ e' :=
    begin
      intros e t ht,
      induction ht,
      case has_type.bool bv {
        apply or.inl,
        exact expr.value.bool bv
      },
      case has_type.nat nv {
        apply or.inl,
        exact expr.value.nat nv
      },
      case has_type.add e₁ e₂ ht₁ ht₂ hp₁ hp₂ {
        exact sorry
      },
      case has_type.mul e₁ e₂ ht₁ ht₂ hp₁ hp₂ {
        exact sorry
      },
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
      induction ht,
      case has_type.bool bv hsbv {
        exact sorry,
      },
      case has_type.nat hsnat {
        exact sorry
      },
      case has_type.add e₁ e₂ ht₁ ht₂ hp₁ hp₂ {
        exact sorry
      },
      case has_type.mul e₁ e₂ ht₁ ht₂ hp₁ hp₂ {
        exact sorry
      },
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
    inductive type (α : Type) : Type
      | bvar {} : ℕ → type
      | fvar {} : α → type
      | unit {} : type
      | bit {} : type
      | sum {} : type → type → type
      | prod {} : type → type → type
      | array {} : type → host.expr → type
      | cond {} : type → host.expr → type
      | abs {} : kind → type → type
      | app {} : type → type → type

    namespace type

      variables {α : Type}

      instance has_coe_from_nat : has_coe ℕ (type α) := ⟨bvar⟩
      instance has_coe_from_atom : has_coe α (type α) := ⟨fvar⟩

      -- Overload the `+` operator for constructing sum types
      instance : has_add (type α) := ⟨type.sum⟩

      def bind {β : Type} : type α → (α → type β) → type β
        | (bvar i)      f := bvar i
        | (fvar x)      f := (f x)
        | (unit)        f := unit
        | (bit)         f := bit
        | (sum t₁ t₂)   f := sum (bind t₁ f) (bind t₂ f)
        | (prod t₁ t₂)  f := prod (bind t₁ f) (bind t₂ f)
        | (array t e)   f := array (bind t f) e
        | (cond t e)    f := cond (bind t f) e
        | (abs k t)     f := abs k (bind t f)
        | (app t₁ t₂)   f := app (bind t₁ f) (bind t₂ f)

      instance : monad type :=
        { pure := @fvar
        , bind := @bind
        , id_map := begin
            intros α t,
            simp [bind, function.comp],
            induction t,
              case bvar i { exact rfl },
              case fvar x { exact rfl },
              case unit { exact rfl },
              case bit { exact rfl },
              case sum t₁ t₂ ht₁ ht₂ {
                simp [bind, function.comp],
                rw [ht₁, ht₂],
              },
              case prod t₁ t₂ ht₁ ht₂ {
                simp [bind, function.comp],
                rw [ht₁, ht₂],
              },
              case array t e ht {
                simp [bind, function.comp],
                rw [ht],
              },
              case cond t e ht {
                simp [bind, function.comp],
                rw [ht],
              },
              case abs k t ht {
                simp [bind, function.comp],
                rw [ht],
              },
              case app t₁ t₂ ht₁ ht₂ {
                simp [bind, function.comp],
                rw [ht₁, ht₂],
              },
          end
        , pure_bind := by intros; apply rfl
        , bind_assoc := begin
            intros α β γ t f g,
            induction t,
              case bvar i { exact rfl },
              case fvar x { exact rfl },
              case unit { exact rfl },
              case bit { exact rfl },
              case sum t₁ t₂ ht₁ ht₂ {
                simp [bind],
                simp [bind] at ht₁ ht₂,
                rw [ht₁, ht₂]
              },
              case prod t₁ t₂ ht₁ ht₂ {
                simp [bind],
                simp [bind] at ht₁ ht₂,
                rw [ht₁, ht₂]
              },
              case array t e ht {
                simp [bind],
                simp [bind] at ht,
                rw [ht]
              },
              case cond t e ht {
                simp [bind],
                simp [bind] at ht,
                rw [ht]
              },
              case abs k t ht {
                simp [bind],
                simp [bind] at ht,
                rw [ht]
              },
              case app t₁ t₂ ht₁ ht₂ {
                simp [bind],
                simp [bind] at ht₁ ht₂,
                rw [ht₁, ht₂]
              },
          end
        }

    end type

    -- Type variables
    prefix `b#`:0 := type.bvar
    prefix `f#`:50 := type.fvar
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


    -- SUBSTITUTION

    namespace type

      variables {α : Type} [decidable_eq α]

      def subst (z : α) (u : type α) (t : type α) : type α :=
        t >>= λ x, if x = z then u else (fvar x)

      notation `[ ` z ` ↦ ` u ` ]` e := subst z u e

      example {x: α} {y : type α} :
        ([x ↦ y] Λ0: ★, ↑0 ∙ ↑x) = (Λ0: ★, ↑0 ∙ y) := sorry

    end type


    -- OPENING/CLOSING

    namespace type

      variables {α : Type} [decidable_eq α]

      def open_var : ℕ → α → type α → type α
        | i x (bvar i') := if i = i' then fvar x else bvar i'
        | i x (fvar x') := fvar x'
        | i x (unit) := unit
        | i x (bit) := bit
        | i x (sum t₁ t₂) := sum (open_var i x t₁) (open_var i x t₂)
        | i x (prod t₁ t₂) := prod (open_var i x t₁) (open_var i x t₂)
        | i x (array t e) := array (open_var i x t) e
        | i x (cond t e) := cond (open_var i x t) e
        | i x (abs k t) := abs k (open_var (i + 1) x t)
        | i x (app t₁ t₂) := app (open_var i x t₁) (open_var i x t₂)

      def close_var : ℕ → α → type α → type α
        | i x (bvar i') := bvar i'
        | i x (fvar x') := if x = x' then bvar i else fvar x'
        | i x (unit) := unit
        | i x (bit) := bit
        | i x (sum t₁ t₂) := sum (close_var i x t₁) (close_var i x t₂)
        | i x (prod t₁ t₂) := prod (close_var i x t₁) (close_var i x t₂)
        | i x (array t e) := array (close_var i x t) e
        | i x (cond t e) := cond (close_var i x t) e
        | i x (abs k t) := abs k (close_var (i + 1) x t)
        | i x (app t₁ t₂) := app (close_var i x t₁) (close_var i x t₂)

      theorem close_open_var :
        Π (x : α) (t : type α),
        close_var 0 x (open_var 0 x t) = t :=
      begin
        intros x t,
        induction t,
          case bvar i { exact sorry },
          case fvar x { exact sorry },
          case unit { exact sorry },
          case bit { exact sorry },
          case sum t₁ t₂ ht₁ ht₂ { exact sorry },
          case prod t₁ t₂ ht₁ ht₂ { exact sorry },
          case array t e ht { exact sorry },
          case cond t e ht { exact sorry },
          case abs k t ht { exact sorry },
          case app t₁ t₂ ht₁ ht₂ { exact sorry },
      end

      theorem open_close_var :
        Π (x : α) (t : type α),
        open_var 0 x (close_var 0 x t) = t :=
      begin
        intros x t,
        induction t,
          case bvar i { exact sorry },
          case fvar x {  exact sorry },
          case unit { exact sorry },
          case bit { exact sorry },
          case sum t₁ t₂ ht₁ ht₂ { exact sorry },
          case prod t₁ t₂ ht₁ ht₂ { exact sorry },
          case array t e ht { exact sorry },
          case cond t e ht { exact sorry },
          case abs k t ht { exact sorry },
          case app t₁ t₂ ht₁ ht₂ { exact sorry },
      end

    end type


    -- CONTEXTS

    def ctx : Type :=
      list kind

    namespace ctx

      def lookup (n : ℕ) (Γ : ctx) : option kind :=
          list.nth Γ n

      def lookup_le (n : ℕ) (Γ : ctx) : n < Γ.length → kind :=
        assume is_le,
          list.nth_le Γ n is_le

    end ctx


    -- KINDING RULES

    inductive has_kind {α : Type} : ctx → type α → kind → Prop
      | var {Γ} (x : ℕ) {k} :
          ctx.lookup x Γ = some k →
          has_kind Γ x k
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

    /- A correctly kinded type -/
    structure kinded_type (α : Type) : Type :=
      (Γ : ctx)
      (t : type α)
      (k : kind)
      (h : has_kind Γ t k)


    -- EMBEDDING

    /- Embed a kind as a Lean term -/
    def kind.embed : kind → Type 1
      | kind.type := Type 0
      | (kind.arrow k₁ k₂) := kind.embed k₁ → kind.embed k₂

    def kinded_type.embed {α : Type} : Π (kt : kinded_type α), kt.k.embed :=
      sorry


    -- BINARY DATA PARSING

    -- FIXME: constrain `kt.embed` to be `Type 0`
    def parse {α : Type} : Π (kt : kinded_type α), list bool → /- kt.embed -/ sorry :=
      sorry

  end binary

end ddl

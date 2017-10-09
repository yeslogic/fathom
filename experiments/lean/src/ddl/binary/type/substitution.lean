import ddl.binary.type.syntax
import ddl.binary.type.monad

namespace ddl.binary.type

  open ddl.binary

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

  def subst (z : α) (u : type α) (t : type α) : type α :=
    t >>= λ x, if x = z then u else (fvar x)

  notation `[ ` z ` ↦ ` u ` ]` e := subst z u e

  example {x: α} {y : type α} :
      ([x ↦ y] Λ0: ★, ↑0 ∙ ↑x) = (Λ0: ★, ↑0 ∙ y) := sorry

end ddl.binary.type

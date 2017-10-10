import ddl.binary.basic
import ddl.binary.monad

namespace ddl.binary

  -- open ddl.binary

  namespace type

    variables {ℓ α : Type} [decidable_eq α]


    def open_var : ℕ → α → type ℓ α → type ℓ α
      | i x (bvar i') := if i = i' then fvar x else bvar i'
      | i x (fvar x') := fvar x'
      | i x (bit) := bit
      | i x (sum t₁ t₂) := sum (open_var i x t₁) (open_var i x t₂)
      | i x (struct_nil) := struct_nil
      | i x (struct_cons l t₁ t₂) := struct_cons l (open_var i x t₁) (open_var i x t₂)
      | i x (array t e) := array (open_var i x t) e
      | i x (cond t e) := cond (open_var i x t) e
      | i x (abs k t) := abs k (open_var (i + 1) x t)
      | i x (app t₁ t₂) := app (open_var i x t₁) (open_var i x t₂)


    def close_var : ℕ → α → type ℓ α → type ℓ α
      | i x (bvar i') := bvar i'
      | i x (fvar x') := if x = x' then bvar i else fvar x'
      | i x (bit) := bit
      | i x (sum t₁ t₂) := sum (close_var i x t₁) (close_var i x t₂)
      | i x (struct_nil) := struct_nil
      | i x (struct_cons l t₁ t₂) := struct_cons l (close_var i x t₁) (close_var i x t₂)
      | i x (array t e) := array (close_var i x t) e
      | i x (cond t e) := cond (close_var i x t) e
      | i x (abs k t) := abs k (close_var (i + 1) x t)
      | i x (app t₁ t₂) := app (close_var i x t₁) (close_var i x t₂)


    theorem close_open_var :
      Π (x : α) (t : type ℓ α),
      close_var 0 x (open_var 0 x t) = t :=
    begin
      intros x t,
      induction t,
        case bvar i { admit },
        case fvar x { admit },
        case bit { admit },
        case sum t₁ t₂ ht₁ ht₂ { admit },
        case struct_nil { admit },
        case struct_cons l t₁ t₂ ht₁ ht₂ { admit },
        case array t e ht { admit },
        case cond t e ht { admit },
        case abs k t ht { admit },
        case app t₁ t₂ ht₁ ht₂ { admit },
    end


    theorem open_close_var :
      Π (x : α) (t : type ℓ α),
      open_var 0 x (close_var 0 x t) = t :=
    begin
      intros x t,
      induction t,
        case bvar i { admit },
        case fvar x {  admit },
        case bit { admit },
        case sum t₁ t₂ ht₁ ht₂ { admit },
        case struct_nil { admit },
        case struct_cons l t₁ t₂ ht₁ ht₂ { admit },
        case array t e ht { admit },
        case cond t e ht { admit },
        case abs k t ht { admit },
        case app t₁ t₂ ht₁ ht₂ { admit },
    end


    def subst (x : α) (src : type ℓ α) (dst : type ℓ α) : type ℓ α :=
      dst >>= λ x', if x' = x then src else (fvar x)

    notation `[ ` x ` ↦ ` src ` ]` dst := subst x src dst

    example {x: α} {t : type ℓ α} :
        ([x ↦ t] Λ0: ★, ↑0 ∙ ↑x) = (Λ0: ★, ↑0 ∙ t) := sorry

  end type

end ddl.binary

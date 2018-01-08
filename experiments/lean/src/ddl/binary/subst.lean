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
      | i x (union_nil) := union_nil
      | i x (union_cons l t₁ t₂) := union_cons l (open_var i x t₁) (open_var i x t₂)
      | i x (struct_nil) := struct_nil
      | i x (struct_cons l t₁ t₂) := struct_cons l (open_var i x t₁) (open_var (i + 1) x t₂)
      | i x (array t e) := array (open_var i x t) e
      | i x (assert t e) := assert (open_var i x t) e
      | i x (interp t e th) := interp (open_var i x t) e th
      | i x (lam k t) := lam k (open_var (i + 1) x t)
      | i x (app t₁ t₂) := app (open_var i x t₁) (open_var i x t₂)


    def close_var : ℕ → α → type ℓ α → type ℓ α
      | i x (bvar i') := bvar i'
      | i x (fvar x') := if x = x' then bvar i else fvar x'
      | i x (bit) := bit
      | i x (union_nil) := union_nil
      | i x (union_cons l t₁ t₂) := union_cons l (close_var i x t₁) (close_var i x t₂)
      | i x (struct_nil) := struct_nil
      | i x (struct_cons l t₁ t₂) := struct_cons l (close_var i x t₁) (close_var (i + 1) x t₂)
      | i x (array t e) := array (close_var i x t) e
      | i x (assert t e) := assert (close_var i x t) e
      | i x (interp t e th) := interp (close_var i x t) e th
      | i x (lam k t) := lam k (close_var (i + 1) x t)
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
        case union_nil { admit },
        case union_cons l t₁ t₂ ht₁ ht₂ { admit },
        case struct_nil { admit },
        case struct_cons l t₁ t₂ ht₁ ht₂ { admit },
        case array t e ht { admit },
        case assert t e ht { admit },
        case interp t e ht hht { admit },
        case lam k t ht { admit },
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
        case union_nil { admit },
        case union_cons l t₁ t₂ ht₁ ht₂ { admit },
        case struct_nil { admit },
        case struct_cons l t₁ t₂ ht₁ ht₂ { admit },
        case array t e ht { admit },
        case assert t e ht { admit },
        case interp t e ht hht { admit },
        case lam k t ht { admit },
        case app t₁ t₂ ht₁ ht₂ { admit },
    end


    def subst (x : α) (src : type ℓ α) (dst : type ℓ α) : type ℓ α :=
      dst >>= λ x', if x' = x then src else (fvar x)

    notation `[ ` x ` ↦ ` src ` ]` dst := subst x src dst

    example {x: α} {t : type ℓ α} :
        ([x ↦ t] type.lam kind.type (↑0 ∙ ↑x)) = (type.lam kind.type (↑0 ∙ t)) := sorry

  end type

end ddl.binary

import ddl.binary.basic

namespace ddl.binary.type

  open ddl
  open ddl.binary

  variables {ℓ α β : Type}


  def bind : type ℓ α → (α → type ℓ β) → type ℓ β
    | (bvar i)              f := bvar i
    | (fvar x)              f := (f x)
    | (bit)                 f := bit
    | (sum t₁ t₂)           f := sum (bind t₁ f) (bind t₂ f)
    | (struct_nil)          f := struct_nil
    | (struct_cons l t₁ t₂) f := struct_cons l (bind t₁ f) (bind t₂ f)
    | (array t e)           f := array (bind t f) e
    | (assert t e)            f := assert (bind t f) e
    | (interp t e th)       f := interp (bind t f) e th
    | (abs k t)             f := abs k (bind t f)
    | (app t₁ t₂)           f := app (bind t₁ f) (bind t₂ f)


  instance : monad (type ℓ) :=
    { pure := @fvar ℓ
    , bind := @bind ℓ
    , id_map := begin
        intros α t,
        simp [bind, function.comp],
        induction t,
          case bvar i { exact rfl },
          case fvar x { exact rfl },
          case bit { exact rfl },
          case sum t₁ t₂ ht₁ ht₂ {
            simp [bind, function.comp],
            rw [ht₁, ht₂],
          },
          case struct_nil { exact rfl },
          case struct_cons l t₁ t₂ ht₁ ht₂ {
            simp [bind, function.comp],
            rw [ht₁, ht₂],
          },
          case array t e ht {
            simp [bind, function.comp],
            rw [ht],
          },
          case assert t e ht {
            simp [bind, function.comp],
            rw [ht],
          },
          case interp t e ht hht {
            simp [bind, function.comp],
            rw [hht],
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
          case bit { exact rfl },
          case sum t₁ t₂ ht₁ ht₂ {
            simp [bind],
            simp [bind] at ht₁ ht₂,
            rw [ht₁, ht₂]
          },
          case struct_nil { exact rfl },
          case struct_cons l t₁ t₂ ht₁ ht₂ {
            simp [bind],
            simp [bind] at ht₁ ht₂,
            rw [ht₁, ht₂]
          },
          case array t e ht {
            simp [bind],
            simp [bind] at ht,
            rw [ht]
          },
          case assert t e ht {
            simp [bind],
            simp [bind] at ht,
            rw [ht]
          },
          case interp t e ht hht {
            simp [bind],
            simp [bind] at hht,
            rw [hht]
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

end ddl.binary.type

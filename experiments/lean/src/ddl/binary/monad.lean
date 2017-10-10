import ddl.binary.basic

namespace ddl.binary.type

  open ddl
  open ddl.binary

  variables {α β : Type}

  def bind : type α → (α → type β) → type β
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

end ddl.binary.type

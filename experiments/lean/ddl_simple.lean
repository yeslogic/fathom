import data.vector

namespace ddl

  namespace range

    structure range : Type :=
      (min max : ℕ)
      (min_le_max : min ≤ max)

    def range.exact (n : ℕ) : range :=
      { min := n
      , max := n
      , min_le_max := le_of_eq rfl
      }

    def range.union (r₁ : range) (r₂ : range) : range :=
      { min := min r₁.min r₂.min
      , max := max r₁.max r₂.max
      , min_le_max := sorry
      }

    def range.add (r₁ : range) (r₂ : range) : range :=
      { min := r₁.min + r₂.min
      , max := r₁.max + r₂.max
      , min_le_max := add_le_add r₁.min_le_max r₂.min_le_max
      }

    def range.mul (r₁ : range) (r₂ : range) : range :=
      { min := r₁.min * r₂.min
      , max := r₁.max * r₂.max
      , min_le_max := sorry
      }

    def range.mem (n : ℕ) (r : range) : Prop :=
      r.min ≤ n ∧ n ≤ r.max

    instance : has_add range := ⟨range.add⟩
    instance : has_mul range := ⟨range.mul⟩
    instance : has_union range := ⟨range.union⟩
    instance : has_mem ℕ range := ⟨range.mem⟩
    instance : has_coe ℕ range := ⟨range.exact⟩

  end range

  /- The host language -/
  namespace host

    /- The type syntax of the host language -/
    inductive type : Type
      | bool : type
      | nat : type


    /- embed a host type into Lean -/
    def type.embed : type → Type
      | type.bool := bool
      | type.nat := ℕ


    /- The expression syntax of the host language -/
    inductive expr : type → Type
      | bool : bool → expr type.bool
      | nat : ℕ → expr type.nat
      | add : expr type.nat → expr type.nat → expr type.nat
      | mul : expr type.nat → expr type.nat → expr type.nat


    instance has_coe_to_bool : has_coe bool (expr type.bool) := ⟨expr.bool⟩
    instance has_coe_to_nat : has_coe ℕ (expr type.nat) := ⟨expr.nat⟩


    /- embed a host expression into Lean -/
    def expr.embed : Π {t}, expr t → type.embed t
      | _ (expr.bool b)    := b
      | _ (expr.nat n)     := n
      | _ (expr.add e₁ e₂) := nat.add (expr.embed e₁) (expr.embed e₂)
      | _ (expr.mul e₁ e₂) := nat.mul (expr.embed e₁) (expr.embed e₂)


    example : expr.embed (expr.add ↑1 ↑2) = (1 + 2 : ℕ) := rfl
    example : expr.embed (expr.mul ↑4 ↑2) = (4 * 2 : ℕ) := rfl

  end host


  /- The binary language -/
  namespace binary

    open range


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


    /- The context in which our types live.

       At the moment types only have access to kinds. Allowing types to also
       depend on values is a great deal more fiddly to implement!
    -/
    def ctx : ℕ → Type :=
      vector kind

    def ctx.lookup {n} : fin n → ctx n → kind :=
      flip vector.nth

    lemma ctx.lookup_zero_eq_head {n is_lt} {Γ : ctx n} {k : kind} :
        ctx.lookup ⟨0, is_lt⟩ (k :: Γ) = k :=
    begin
      simp [ctx.lookup, flip],
      exact sorry
    end


    /- The type system of the binary language -/
    inductive type : Π {n}, ctx n → kind → Type
      | var   {n} {Γ : ctx n} (x : fin n) : type Γ (ctx.lookup x Γ)
      | unit  {n} {Γ : ctx n}             : type Γ ★
      | bit   {n} {Γ : ctx n}             : type Γ ★
      | sum   {n} {Γ : ctx n}             : type Γ ★ → type Γ ★ → type Γ ★
      | prod  {n} {Γ : ctx n}             : type Γ ★ → type Γ ★ → type Γ ★
      | array {n} {Γ : ctx n}             : type Γ ★ → host.expr host.type.nat → type Γ ★
      | abs   {n} {Γ : ctx n} {k₁ k₂}     : type (k₁ :: Γ) k₂ → type Γ (k₁ ⇒ k₂)
      | app   {n} {Γ : ctx n} {k₁ k₂}     : type Γ (k₁ ⇒ k₂) → type Γ k₁ → type Γ k₂ -- FIXME: pop type from Γ?

    notation `Λ→ ` t := type.abs t
    notation `#` x := type.var x
    infixl ` ∙ `:50 := type.app


    def type.closed : kind → Type :=
      type vector.nil

    example : type.closed ★           := type.prod type.bit type.bit
    example : type.closed ★           := type.array type.bit ↑16
    example : type.closed (★ ⇒ ★)     := (Λ→ #0)
    example : type.closed ★           := (Λ→ #0) ∙ type.bit
    example : type.closed (★ ⇒ ★)     := (Λ→ #0) ∙ (Λ→ #0)
    example : type.closed (★ ⇒ ★ ⇒ ★) := (Λ→ Λ→ type.sum (#1) (#0))
    example : type.closed (★ ⇒ ★)     := (Λ→ Λ→ type.sum (#1) (#0)) ∙ type.bit


    inductive env : Π {n}, ctx n → Type 1
      | nil : env vector.nil
      | cons {n} {Γ : ctx n} {k} : kind.embed k → env Γ → env (k :: Γ)

    notation a :: b := env.cons a b
    notation `[` eΓ:(foldr `, ` (α αs, env.cons α αs) env.nil `]`) := eΓ

    def env.lookup : Π {n} {Γ : ctx n} (m : fin n), env Γ → kind.embed (ctx.lookup m Γ)
      | n .(vector.nil) m          env.nil   := fin.elim0 m
      | n .(_ :: _)     ⟨0,     h⟩ (α :: eΓ) := by { rewrite ctx.lookup_zero_eq_head, exact α }
      | n .(_ :: _)     ⟨m + 1, h⟩ (_ :: eΓ) := sorry


    /- embed a binary type into Lean -/
    def type.embed : Π {n} {Γ : ctx n} (eΓ : env Γ) {k : kind}, type Γ k → kind.embed k
      | n Γ eΓ k (type.var x)        := env.lookup x eΓ
      | n Γ eΓ k type.unit           := unit
      | n Γ eΓ k type.bit            := bool
      | n Γ eΓ k (type.sum t₁ t₂)    := sum (type.embed eΓ t₁) (type.embed eΓ t₂)
      | n Γ eΓ k (type.prod t₁ t₂)   := type.embed eΓ t₁ × type.embed eΓ t₂
      | n Γ eΓ k (type.array t len)  := vector (type.embed eΓ t) (host.expr.embed len)
      | n Γ eΓ k (type.abs t)        := λ x, type.embed (x :: eΓ) t
      | n Γ eΓ k (type.app t₁ t₂)    := (type.embed eΓ t₁) (type.embed eΓ t₂)

    example : type.embed [] (type.prod type.bit type.bit)                       = (bool × bool)  := rfl
    example : type.embed [] (type.array type.bit ↑16)                           = vector bool 16 := rfl
    example : type.embed [] ((Λ→ #0) ∙ type.bit)                                = bool           := rfl
    example : type.embed [] ((Λ→ Λ→ #0) ∙ type.bit ∙ type.unit)                 = unit           := rfl
    --                        /----------------\
    --                        |  /-------------|----\
    --                        |  |             |    |
    --                        v  v             |    |
    example : type.embed [] ((Λ→ Λ→ type.sum (#1) (#0)) ∙ type.bit ∙ type.unit) = (bool × unit)  := rfl


    /- Get the range of bits that a type might occupy -/
    def type.size : Π {n} (Γ : ctx n) {k : kind}, type Γ k → range
      | n Γ k (type.var x)        := sorry
      | n Γ k type.unit           := ↑0
      | n Γ k type.bit            := ↑1
      | n Γ k (type.sum t₁ t₂)    := type.size Γ t₁ ∪ type.size Γ t₂
      | n Γ k (type.prod t₁ t₂)   := type.size Γ t₁ + type.size Γ t₂
      | n Γ k (type.array t len)  := type.size Γ t * range.exact (host.expr.embed len)
      | n Γ k (type.abs t)        := type.size _ t
      | n Γ k (type.app t₁ t₂)    := type.size _ t₁

    example : type.size vector.nil (type.prod type.bit type.bit)  =  ↑2 := rfl
    example : type.size vector.nil (type.prod type.bit type.unit) =  ↑1 := rfl
    example : type.size vector.nil (type.array type.bit ↑16)      = ↑16 := rfl


    /- Read a stream of bits into a Lean term using the type to control
       deserialization
    -/
    def read_bits : Π {n} {Γ : ctx n} (eΓ : env Γ) {k} (t : type Γ k), list bool → type.embed eΓ t
      | n Γ eΓ k (type.var x)        buf       := sorry
      | n Γ eΓ k type.unit           buf       := unit.star
      | n Γ eΓ k type.bit            []        := sorry
      | n Γ eΓ k type.bit            (x :: xs) := x
      | n Γ eΓ k (type.sum t₁ t₂)    buf       := sorry
      | n Γ eΓ k (type.prod t₁ t₂)   buf       := sorry
      | n Γ eΓ k (type.array t len)  buf       := sorry
      | n Γ eΓ k (type.abs _)        buf       := sorry
      | n Γ eΓ k (type.app _ _)      buf       := sorry

  end binary

end ddl

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

    /- The expression syntax of the host language -/
    inductive expr : type → Type
      | bool : bool → expr type.bool
      | nat : ℕ → expr type.nat
      | add : expr type.nat → expr type.nat → expr type.nat
      | mul : expr type.nat → expr type.nat → expr type.nat

    instance has_coe_to_bool : has_coe bool (expr type.bool) := ⟨expr.bool⟩
    instance has_coe_to_nat : has_coe ℕ (expr type.nat) := ⟨expr.nat⟩

    /- embed a host type into Lean -/
    def type.embed : type → Type
      | type.bool := bool
      | type.nat := ℕ

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

    /- The environment in which our types live.

       At the moment types only have access to kinds. Allowing types to also
       depend on values is a great deal more fiddly to implement!
    -/
    def env : ℕ → Type :=
      vector kind

    def env.lookup {n} : fin n → env n → kind :=
      flip vector.nth

    /- The type system of the binary language -/
    inductive type : Π {n}, env n → kind → Type
      | var   {n} {Γ : env n} (x : fin n) : type Γ (env.lookup x Γ)
      | unit  {n} {Γ : env n}             : type Γ ★
      | bit   {n} {Γ : env n}             : type Γ ★
      | sum   {n} {Γ : env n}             : type Γ ★ → type Γ ★ → type Γ ★
      | prod  {n} {Γ : env n}             : type Γ ★ → type Γ ★ → type Γ ★
      | array {n} {Γ : env n}             : type Γ ★ → host.expr host.type.nat → type Γ ★
      | abs   {n} {Γ : env n} {k₁ k₂}     : type (k₁ :: Γ) k₂ → type Γ (k₁ ⇒ k₂)
      | app   {n} {Γ : env n} {k₁ k₂}     : type Γ (k₁ ⇒ k₂) → type Γ k₁ → type Γ k₂ -- FIXME: pop type from Γ?

    /- embed a binary type into Lean -/
    def type.embed : Π {n} (Γ : env n) {k : kind}, type Γ k → kind.embed k
      | n Γ k (type.var x)        := sorry
      | n Γ k type.unit           := unit
      | n Γ k type.bit            := bool
      | n Γ k (type.sum t₁ t₂)    := sum (type.embed Γ t₁) (type.embed Γ t₂)
      | n Γ k (type.prod t₁ t₂)   := type.embed Γ t₁ × type.embed Γ t₂
      | n Γ k (type.array t len)  := vector (type.embed Γ t) (host.expr.embed len)
      | n Γ k (type.abs t)        := λ x, type.embed (_ :: Γ) t
      | n Γ k (type.app t₁ t₂)    := (type.embed Γ t₁) (type.embed Γ t₂)

    example : type.embed vector.nil (type.prod type.bit type.bit) = (bool × bool) := rfl
    example : type.embed vector.nil (type.array type.bit ↑16) = vector bool 16 := rfl

    def type.size : Π {n} (Γ : env n) {k : kind}, type Γ k → range
      | n Γ k (type.var x)        := sorry
      | n Γ k type.unit           := ↑0
      | n Γ k type.bit            := ↑1
      | n Γ k (type.sum t₁ t₂)    := type.size Γ t₁ ∪ type.size Γ t₂
      | n Γ k (type.prod t₁ t₂)   := type.size Γ t₁ + type.size Γ t₂
      | n Γ k (type.array t len)  := type.size Γ t * range.exact (host.expr.embed len)
      | n Γ k (type.abs t)        := type.size _ t
      | n Γ k (type.app t₁ t₂)    := type.size _ t₁

    example : type.size vector.nil (type.prod type.bit type.bit) = ↑2 := rfl
    example : type.size vector.nil (type.prod type.bit type.unit) = ↑1 := rfl
    example : type.size vector.nil (type.array type.bit ↑16) = ↑16 := rfl

    def read_bits :
      Π {n : ℕ} (Γ : env n)
        {k : kind} (t : type Γ k)
        (buf : list bool) {h : list.length buf ∈ type.size Γ t},
        type.embed Γ t
      | n Γ k (type.var x)        buf       h := sorry
      | n Γ k type.unit           buf       h := unit.star
      | n Γ k type.bit            []        h := sorry
      | n Γ k type.bit            (x :: xs) h := x
      | n Γ k (type.sum t₁ t₂)    buf       h := sorry
      | n Γ k (type.prod t₁ t₂)   buf       h := sorry
      | n Γ k (type.array t len)  buf       h := sorry
      | n Γ k (type.abs _)        buf       h := sorry
      | n Γ k (type.app _ _)      buf       h := sorry

  end binary

end ddl

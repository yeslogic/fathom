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

    /- The type system of the binary language -/
    inductive type : Type
      | unit : type
      | bit : type
      | sum : type → type → type
      | prod : type → type → type
      | array : type → host.expr host.type.nat → type

    /- embed a binary type into Lean -/
    def type.embed : type → Type
      | type.unit := unit
      | type.bit := bool
      | (type.sum t₁ t₂) := sum (type.embed t₁) (type.embed t₂)
      | (type.prod t₁ t₂) := type.embed t₁ × type.embed t₂
      | (type.array t len) := vector (type.embed t) (host.expr.embed len)

    example : type.embed (type.prod type.bit type.bit) = (bool × bool) := rfl
    example : type.embed (type.array type.bit ↑16) = vector bool 16 := rfl

    def type.size : type → range
      | type.unit := ↑0
      | type.bit := ↑1
      | (type.sum t₁ t₂) := type.size t₁ ∪ type.size t₂
      | (type.prod t₁ t₂) := type.size t₁ + type.size t₂
      | (type.array t len) := type.size t * range.exact (host.expr.embed len)

    example : type.size (type.prod type.bit type.bit) = ↑2 := rfl
    example : type.size (type.prod type.bit type.unit) = ↑1 := rfl
    example : type.size (type.array type.bit ↑16) = ↑16 := rfl

    def read_bytes : Π (t : type) (buf : list bool) {h : list.length buf ∈ type.size t}, type.embed t :=
      sorry

  end binary

end ddl

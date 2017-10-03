import data.vector

namespace ddl

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

    example : expr.embed (expr.add (expr.nat 1) (expr.nat 2)) = (1 + 2 : ℕ) := rfl
    example : expr.embed (expr.mul (expr.nat 4) (expr.nat 2)) = (4 * 2 : ℕ) := rfl

  end host


  /- The binary language -/
  namespace binary

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
    example : type.embed (type.array type.bit (host.expr.nat 16)) = vector bool 16 := rfl

    def type.max_bits : type → ℕ
      | type.unit := 0
      | type.bit := 1
      | (type.sum t₁ t₂) := max (type.max_bits t₁) (type.max_bits t₂)
      | (type.prod t₁ t₂) := type.max_bits t₁ + type.max_bits t₂
      | (type.array t len) := type.max_bits t * host.expr.embed len

  end binary

end ddl

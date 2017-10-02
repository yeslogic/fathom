import data.vector

namespace ddl

  /- The host language -/
  namespace host

    /- The expression syntax of the host language -/
    inductive expr : Type
      -- | bool : bool → expr
      | nat : ℕ → expr
      | add : expr → expr → expr
      | mul : expr → expr → expr


    /- The type syntax of the host language -/
    inductive type : Type
      | bool : type
      | nat : type


    /- embed a host expression into Lean -/
    def expr.embed : expr → ℕ
      | (expr.nat n) := n
      | (expr.add e₁ e₂) := expr.embed e₁ + expr.embed e₂
      | (expr.mul e₁ e₂) := expr.embed e₁ * expr.embed e₂

    example : expr.embed (expr.add (expr.nat 1) (expr.nat 2)) = 1 + 2 := rfl
    example : expr.embed (expr.mul (expr.nat 4) (expr.nat 2)) = 4 * 2 := rfl

  end host


  /- The binary language -/
  namespace binary

    /- The type system of the binary language -/
    inductive type : Type
      | bit : type
      | sum : type → type → type
      | prod : type → type → type
      | array : type → host.expr → type


    /- embed a binary type into Lean -/
    def type.embed : type → Type
      | type.bit := bool
      | (type.sum t₁ t₂) := sum (type.embed t₁) (type.embed t₂)
      | (type.prod t₁ t₂) := type.embed t₁ × type.embed t₂
      | (type.array t len) := vector (type.embed t) (host.expr.embed len)

    example : type.embed (type.prod type.bit type.bit) = (bool × bool) := rfl
    example : type.embed (type.array type.bit (host.expr.nat 16)) = vector bool 16 := rfl

  end binary

end ddl

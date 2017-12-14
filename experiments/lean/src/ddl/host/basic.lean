import ddl.basic
import ddl.host.arith

namespace ddl.host

  /- The type syntax of the host language -/
  inductive type (ℓ : Type) : Type
    | bool {} : type
    | arith {} : arith.type → type
    | union_nil {} : type
    | union_cons : ℓ → type → type → type
    | struct_nil {} : type
    | struct_cons : ℓ → type → type → type
    | array {} : type → type

  namespace type

    variables {ℓ : Type}

    def lookup (l : ℓ) [decidable_eq ℓ] : type ℓ → option (type ℓ)
      | (struct_cons l' t tr) := if l = l' then some t else lookup tr
      | _                     := none

  end type


  /- The expression syntax of the host language -/
  inductive expr (ℓ : Type) : Type
    | bool {} : bool → expr
    | arith {} : arith.expr → expr
    | unop {} : arith.unop → expr → expr
    | binop {} : arith.binop → expr → expr → expr
    | proj : expr → ℓ → expr

  instance has_coe_to_bool {ℓ} : has_coe bool (expr ℓ) := ⟨expr.bool⟩
  instance has_coe_to_arith {ℓ} : has_coe arith.expr (expr ℓ)  := ⟨expr.arith⟩

  namespace expr

    variables {ℓ : Type}

    instance : has_neg (expr ℓ) := ⟨unop arith.unop.neg⟩
    instance : has_add (expr ℓ) := ⟨binop arith.binop.add⟩
    instance : has_sub (expr ℓ) := ⟨binop arith.binop.sub⟩
    instance : has_mul (expr ℓ) := ⟨binop arith.binop.mul⟩

  end expr

end ddl.host

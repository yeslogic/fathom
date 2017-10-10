import ddl.basic

namespace ddl.host

  /- The type syntax of the host language -/
  inductive type (ℓ : Type) : Type
    | bool {} : type
    | nat {} : type
    | sum {} : type → type → type
    | struct_nil {} : type
    | struct_cons : ℓ → type → type → type
    | array {} : type → type

  namespace type

    variables {ℓ : Type}

    instance : has_add (type ℓ) := ⟨type.sum⟩

    def lookup (l : ℓ) [decidable_eq ℓ] : type ℓ → option (type ℓ)
      | (struct_cons l' t tr) := if l = l' then some t else lookup tr
      | _                     := none

  end type


  /- Binary operators -/
  inductive binop : Type
    | add
    | mul


  /- The expression syntax of the host language -/
  inductive expr (ℓ : Type) : Type
    | bool {} : bool → expr
    | nat {} : ℕ → expr
    | app_binop {} : binop → expr → expr → expr
    | proj : expr → ℓ → expr

  instance has_coe_to_bool {ℓ} : has_coe bool (expr ℓ) := ⟨expr.bool⟩
  instance has_coe_to_nat {ℓ} : has_coe ℕ (expr ℓ)  := ⟨expr.nat⟩

  namespace expr

    variables {ℓ : Type}

    instance : has_add (expr ℓ) := ⟨app_binop binop.add⟩
    instance : has_mul (expr ℓ) := ⟨app_binop binop.mul⟩

  end expr

end ddl.host

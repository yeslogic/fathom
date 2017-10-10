import ddl.basic

namespace ddl.host

  /- The type syntax of the host language -/
  inductive type : Type
    | unit : type
    | bool : type
    | nat : type
    | sum : type → type → type
    | prod : type → type → type
    | array : type → type

  namespace type

    instance : has_add type := ⟨type.sum⟩
    instance : has_mul type := ⟨type.prod⟩

  end type


  /- Binary operators -/
  inductive binop : Type
    | add
    | mul


  /- The expression syntax of the host language -/
  inductive expr : Type
    | bool : bool → expr
    | nat : ℕ → expr
    | app_binop : binop → expr → expr → expr

  instance has_coe_to_bool : has_coe bool expr := ⟨expr.bool⟩
  instance has_coe_to_nat : has_coe ℕ expr  := ⟨expr.nat⟩

  namespace expr

    instance : has_add expr := ⟨app_binop binop.add⟩
    instance : has_mul expr := ⟨app_binop binop.mul⟩

  end expr

end ddl.host

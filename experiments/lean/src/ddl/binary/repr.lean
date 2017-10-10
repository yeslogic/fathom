import ddl.host.basic
import ddl.binary.basic

namespace ddl.binary

  open ddl
  open ddl.binary

  namespace type

    variables {ℓ α : Type}


    def repr : type ℓ α → host.type ℓ
      | (sum t₁ t₂) := t₁.repr + t₂.repr
      | (struct_nil) := host.type.struct_nil
      | (struct_cons l t₁ t₂) := host.type.struct_cons l t₁.repr t₂.repr
      | (array t e) := host.type.array (t.repr)
      | (cond t e) := t.repr
      | _ := sorry

    notation `⟦` t `⟧` := repr t

  end type

end ddl.binary

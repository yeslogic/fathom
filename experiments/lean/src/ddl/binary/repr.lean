import ddl.host.basic
import ddl.binary.basic

namespace ddl.binary

  open ddl
  open ddl.binary

  namespace type

    variables {α : Type}

    def repr : type α → host.type
      | (unit) := host.type.unit
      | (sum t₁ t₂) := t₁.repr + t₂.repr
      | (prod t₁ t₂) := t₁.repr * t₂.repr
      | (array t e) := host.type.array (t.repr)
      | (cond t e) := t.repr
      | _ := sorry

    notation `⟦` t `⟧` := repr t

  end type

end ddl.binary

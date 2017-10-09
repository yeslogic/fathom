import ddl.host
import ddl.binary.type.syntax

namespace ddl.binary.type

  open ddl
  open ddl.binary

  variables {α : Type}

  def repr : type α → host.type
    | (unit) := host.type.unit
    | (sum t₁ t₂) := t₁.repr + t₂.repr
    | (prod t₁ t₂) := t₁.repr * t₂.repr
    | (array t e) := host.type.array (t.repr)
    | (cond t e) := t.repr
    | _ := sorry

  notation `⟦` t `⟧` := repr t

end ddl.binary.type

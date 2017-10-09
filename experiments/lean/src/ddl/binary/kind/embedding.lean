import ddl.binary.kind.syntax

namespace ddl.binary.kind

  open ddl.binary

  /- Embed a kind as a Lean term -/
  def embed : kind → Type 1
    | type := Type 0
    | (arrow k₁ k₂) := embed k₁ → embed k₂

end ddl.binary.kind

import ddl.binary.kind.embedding
import ddl.binary.type.semantics

namespace ddl.binary.type

  open ddl.binary

  variables {α : Type}

  def kinded.embed : Π (tk : type.kinded α), tk.k.embed :=
    sorry

end ddl.binary.type

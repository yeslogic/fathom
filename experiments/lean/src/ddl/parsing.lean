import ddl.binary

namespace ddl

  -- FIXME: constrain `kt.embed` to be `Type 0`
  def parse {α : Type} : Π (kt : binary.type.kinded α), list bool → /- kt.embed -/ sorry :=
    sorry

end ddl

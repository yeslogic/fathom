import ddl.binary

namespace ddl

  -- FIXME: constrain `kt.embed` to be `Type 0`
  def parse {ℓ α} [decidable_eq ℓ] : Π (kt : binary.kinded_type ℓ α), list bool → /- kt.embed -/ sorry :=
    sorry

end ddl

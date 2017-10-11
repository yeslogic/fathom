import ddl.host.basic
import ddl.binary.basic

namespace ddl

  open ddl

  inductive binder (ℓ : Type) : Type
    | struct : host.type ℓ → binder
    | abs {} : binary.kind → binder

end ddl

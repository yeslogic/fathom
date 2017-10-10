import ddl.host.basic
import ddl.binary.basic

namespace ddl.binary

  open ddl

  inductive binder (ℓ : Type) : Type
    | prod : host.type ℓ → binder
    | abs {} : kind → binder

  prefix `Σ`:max := binder.prod
  prefix `Λ`:max := binder.abs

end ddl.binary

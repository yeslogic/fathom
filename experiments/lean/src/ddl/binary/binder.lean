import ddl.host
import ddl.binary.kind.syntax

namespace ddl.binary

  open ddl

  inductive binder : Type
    | prod : host.type → binder
    | abs : kind → binder

  prefix `Σ`:max := binder.prod
  prefix `Λ`:max := binder.abs

end ddl.binary

import ddl.binary.binder

namespace ddl.binary


  def ctx (ℓ : Type) : Type :=
    list (binder ℓ)

  namespace ctx

    variables {ℓ : Type}

    def lookup (n : ℕ) (Γ : ctx ℓ) : option (binder ℓ) :=
        list.nth Γ n

    def lookup_le (n : ℕ) (Γ : ctx ℓ) : n < Γ.length → binder ℓ :=
      assume is_le,
        list.nth_le Γ n is_le

  end ctx

end ddl.binary

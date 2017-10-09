import ddl.binary.binder

namespace ddl.binary

  def ctx : Type :=
    list binder

  namespace ctx

    def lookup (n : ℕ) (Γ : ctx) : option binder :=
        list.nth Γ n

    def lookup_le (n : ℕ) (Γ : ctx) : n < Γ.length → binder :=
      assume is_le,
        list.nth_le Γ n is_le

  end ctx

end ddl.binary

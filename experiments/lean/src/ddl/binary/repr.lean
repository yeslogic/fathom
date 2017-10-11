import ddl.binary.basic
import ddl.binary.formation
import ddl.host.basic
import ddl.host.formation

namespace ddl.binary

  open ddl
  open ddl.binary

  namespace type

    variables {ℓ α : Type}


    def repr : type ℓ α → host.type ℓ
      | (sum t₁ t₂)           := t₁.repr + t₂.repr
      | (struct_nil)          := host.type.struct_nil
      | (struct_cons l t₁ t₂) := host.type.struct_cons l t₁.repr t₂.repr
      | (array t e)           := host.type.array (t.repr)
      | (cond t e)            := t.repr
      | (interp t e ht)       := ht
      | _                     := sorry


    lemma repr_well_formed [decidable_eq ℓ] :
        Π (t : binary.type ℓ α),
        well_formed t →
        host.type.well_formed t.repr :=
      begin
        intros bt hbtwf,
        induction hbtwf,
          case well_formed.bvar i { admit },
          case well_formed.fvar x { admit },
          case well_formed.bit { admit },
          case well_formed.sum t₁ t₂ { admit },
          case well_formed.struct_nil {
            exact host.type.well_formed.struct_nil
          },
          case well_formed.struct_cons l t₁ t₂ hbtwf₁ hbtwf₂ hbts₂ hhtwf₁ hhtwf₂ {
            exact host.type.well_formed.struct_cons hhtwf₁ hhtwf₂ sorry
          },
          case well_formed.array t₁ e hbtwf₁ hhtwf₁ {
            exact host.type.well_formed.array hhtwf₁,
          },
          case well_formed.cond t₁ e hbtwf₁ hhtwf₁ {
            exact hhtwf₁,
          },
          case well_formed.interp t₁ e ht hbtwf₁ hhtwf₁ {
            simp [repr],
            admit,
          },
          case well_formed.abs t₁ k hbtwf₁ { admit },
          case well_formed.app t₁ t₂ hbtwf₁ hbtwf₂ { admit },
      end

  end type

end ddl.binary

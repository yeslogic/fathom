import ddl.host.typing
import ddl.binary.basic

namespace ddl.binary

  open ddl
  open ddl.binary

  namespace type

    variables {ℓ α : Type} [decidable_eq ℓ]


    inductive struct : type ℓ α → Prop
      | nil {} : struct struct_nil
      | cons {l t₁ t₂} : struct (struct_cons l t₁ t₂)


    inductive well_formed : type ℓ α → Prop
      | bvar {} (i) :
          well_formed (bvar i)
      | fvar (x) :
          well_formed (fvar x)
      | bit {} :
          well_formed bit
      | sum {t₁ t₂} :
          well_formed t₁ →
          well_formed t₂ →
          well_formed (sum t₁ t₂)
      | struct_nil {} :
          well_formed struct_nil
      | struct_cons {l t₁ t₂} :
          well_formed t₁ →
          well_formed t₂ →
          struct t₂ →
          well_formed (struct_cons l t₁ t₂)
      | array {t e} :
          well_formed t →
          well_formed (array t e)
      | assert {t e} :
          well_formed t →
          well_formed (assert t e)
      | interp {t e th} :
          well_formed t →
          well_formed (interp t e th)
      | abs {t k} :
          well_formed t →
          well_formed (abs k t)
      | app {t₁ t₂} :
          well_formed t₁ →
          well_formed t₂ →
          well_formed (app t₁ t₂)


    lemma well_formed_lookup :
        Π {l : ℓ} {tr tf : type ℓ α},
        well_formed tr →
        lookup l tr = some tf →
        well_formed tf :=
      begin
        admit
      end


  end type

end ddl.binary

import ddl.host.typing

namespace ddl.host

  open ddl
  open ddl.host

  namespace type

    variables {ℓ : Type} [decidable_eq ℓ]


    inductive struct : type ℓ → Prop
      | nil {} : struct struct_nil
      | cons {l t₁ t₂} : struct (struct_cons l t₁ t₂)


    inductive well_formed : type ℓ → Prop
      | bool {} :
          well_formed bool
      | nat {} :
          well_formed nat
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
      | array {t} :
          well_formed t →
          well_formed (array t)


    lemma well_formed_lookup :
        Π {l : ℓ} {tr tf : type ℓ},
        well_formed tr →
        lookup l tr = some tf →
        well_formed tf :=
      begin
        admit
      end


  end type

end ddl.host

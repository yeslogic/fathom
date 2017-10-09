import ddl.host.basic

namespace ddl.host

  -- PROGRESS
  -- https://softwarefoundations.cis.upenn.edu/plf-current/StlcProp.html#lab220

  theorem progress :
    Π (e : expr) (t : type),
    has_type e t →
    value e ∨ ∃ e', e ⟹ e' :=
  begin
    intros e t ht,
    induction ht,
      case has_type.bool bv {
        apply or.inl,
        exact value.bool bv
      },
      case has_type.nat nv {
        apply or.inl,
        exact value.nat nv
      },
      case has_type.add e₁ e₂ ht₁ ht₂ hp₁ hp₂ {
        exact sorry
      },
      case has_type.mul e₁ e₂ ht₁ ht₂ hp₁ hp₂ {
        exact sorry
      },
  end


  -- PRESERVATION
  -- https://softwarefoundations.cis.upenn.edu/plf-current/StlcProp.html#lab222

  theorem preservation :
    Π (e e' : expr) (t : type),
    has_type e t →
    e ⟹ e' →
    has_type e' t :=
  begin
    intros e e' t ht hs,
    induction ht,
      case has_type.bool bv hsbv {
        exact sorry,
      },
      case has_type.nat hsnat {
        exact sorry
      },
      case has_type.add e₁ e₂ ht₁ ht₂ hs₁ hs₂ {
        exact sorry
      },
      case has_type.mul e₁ e₂ ht₁ ht₂ hs₁ hs₂ {
        exact sorry
      },
  end

end ddl.host

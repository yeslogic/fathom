import ddl.binary.formation
import ddl.binary.repr
import ddl.host.formation
import ddl.host.evaluation
import ddl.host.typing

namespace ddl.host

  open ddl

  variables {ℓ α : Type} [decidable_eq ℓ]


  -- WELL-FORMEDNESS
  -- https://softwarefoundations.cis.upenn.edu/plf-current/Records.html#lab348

  lemma has_type_well_formed :
      Π (e : expr ℓ) (t : type ℓ),
      has_type e t →
      type.well_formed t :=
    begin
      admit
    end


  -- WELL-FORMEDNESS
  -- https://softwarefoundations.cis.upenn.edu/plf-current/Records.html#lab348

  lemma lookup_field_in_value :
      Π (v : expr ℓ) (l : ℓ) (tr tf : type ℓ),
      value v →
      has_type v tr →
      type.lookup l tr = some tf →
      ∃ ef, /- expr.lookup l v = some ef ∧ -/ has_type ef tf :=
    begin
      admit
    end

  -- PROGRESS
  -- https://softwarefoundations.cis.upenn.edu/plf-current/StlcProp.html#lab220

  theorem progress :
      Π (e : expr ℓ) (t : type ℓ),
      has_type e t →
      value e ∨ ∃ e', e ⟹ e' :=
    begin
      intros e t ht,
      induction ht,
        case has_type.bool bv {
          apply or.inl,
          exact value.bool bv
        },
        case has_type.arith nv {
          admit
        },
        case has_type.neg e₁ ht₁ hp₁ {
          admit
        },
        case has_type.add e₁ e₂ ht₁ ht₂ hp₁ hp₂ {
          admit
        },
        case has_type.sub e₁ e₂ ht₁ ht₂ hp₁ hp₂ {
          admit
        },
        case has_type.mul e₁ e₂ ht₁ ht₂ hp₁ hp₂ {
          admit
        },
        case has_type.proj e₁ tr tf l ht₁ hl hp₁ {
          admit
        }
    end


  -- PRESERVATION
  -- https://softwarefoundations.cis.upenn.edu/plf-current/StlcProp.html#lab222

  theorem preservation :
      Π (e e' : expr ℓ) (t : type ℓ),
      has_type e t →
      e ⟹ e' →
      has_type e' t :=
    begin
      intros e e' t ht hs,
      induction ht,
        case has_type.bool bv hsbv {
          admit,
        },
        case has_type.arith hsarith {
          admit,
        },
        case has_type.neg e₁ ht₁ hs₁ {
          admit,
        },
        case has_type.add e₁ e₂ ht₁ ht₂ hs₁ hs₂ {
          admit,
        },
        case has_type.sub e₁ e₂ ht₁ ht₂ hs₁ hs₂ {
          admit,
        },
        case has_type.mul e₁ e₂ ht₁ ht₂ hs₁ hs₂ {
          admit,
        },
        case has_type.proj e₁ tr tf l ht₁ hl hs₁ {
          admit,
        }
    end

end ddl.host

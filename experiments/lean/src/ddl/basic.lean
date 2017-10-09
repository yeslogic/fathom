namespace ddl

  def relation (α : Type) :=
    α → α → Prop

  inductive multi {α : Type} (ρ : relation α) : relation α
    | refl {x : α} : multi x x
    | step {x y z : α} : ρ x y → multi y z → multi x z

end ddl

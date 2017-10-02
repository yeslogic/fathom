namespace prefix_list

inductive prefix_list {t} : list t → list t → Prop
  | nil {bs} : prefix_list list.nil bs
  | cons (as b bs) : prefix_list as bs → prefix_list (b :: as) (b :: bs)

theorem prefix_list_refl {t} {as:list t} :
    prefix_list as as :=
begin
  induction as,
  constructor,
  constructor,
  assumption
end

theorem prefix_list_append_right {t} {as bs cs:list t} :
    prefix_list as bs →
    prefix_list as (bs ++ cs) :=
begin
  intro h,
  induction h,
  {
      constructor
  },
  {
      rewrite list.cons_append,
      constructor,
      assumption
  }
end

theorem prefix_list_split {t} {as bs:list t} :
    prefix_list as bs →
    exists cs, as ++ cs = bs :=
begin
  intro h,
  induction h,
  {
      existsi bs_1,
      refl
  },
  {
      cases ih_1,
      existsi a_1,
      rewrite list.cons_append,
      rewrite a_2
  }
end

theorem prefix_list_trans {t} {as bs cs:list t} :
    prefix_list as bs →
    prefix_list bs cs →
    prefix_list as cs :=
begin
  intros h1 h2,
  cases (prefix_list_split h2) with bs' h3,
  rewrite ← h3,
  apply prefix_list_append_right,
  apply h1
end

theorem prefix_list_append_left {t} {as bs cs:list t} :
    prefix_list (as ++ bs) cs →
    prefix_list as cs :=
begin
  intro h,
  apply (@prefix_list_trans t as (as ++ bs) cs),
  apply prefix_list_append_right,
  apply prefix_list_refl,
  assumption
end

inductive chop {t} {R:list t → Prop} : list t → list t → Prop
  | chop {as bs} :
    R as →
    prefix_list as bs →
    (∀as' bs', R as' ∧ prefix_list as' bs' ∧ prefix_list bs bs' → as' = as) →
    chop as bs

theorem chop_extend {t} {R:list t → Prop} {as bs cs:list t} :
    @chop t R as bs →
    @chop t R as (bs ++ cs) :=
begin
  intro h1,
  cases h1 with _ _ h2 h3 h4,
  constructor,
  assumption,
  {
      apply prefix_list_append_right,
      assumption
  },
  {
      intros as' bs' h5,
      cases h5 with h6 h7,
      cases h7 with h8 h9,
      apply (h4 as' bs'),
      split,
      assumption,
      split,
      assumption,
      {
          apply (@prefix_list_append_left t bs cs bs'),
          assumption
      }
  }
end

end prefix_list

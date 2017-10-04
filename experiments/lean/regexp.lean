namespace regexp

def symbol:Type := nat

inductive regexp : Type
  | zero : regexp
  | one : regexp
  | sym : symbol → regexp
  | alt : regexp → regexp → regexp
  | cat : regexp → regexp → regexp
  | star : regexp → regexp

inductive matches : regexp → list symbol → Prop
  | one : matches regexp.one list.nil
  | sym {a} : matches (regexp.sym a) (a :: list.nil)
  | alt_l {ra rb xs} : matches ra xs → matches (regexp.alt ra rb) xs
  | alt_r {ra rb xs} : matches rb xs → matches (regexp.alt ra rb) xs
  | cat {ra rb xs ys zs} :
      matches ra xs →
      matches rb ys →
      zs = xs ++ ys →
      matches (regexp.cat ra rb) zs
  | star_nil {r} : matches (regexp.star r) list.nil
  | star_cons {r xs ys} :
      matches r xs →
      matches (regexp.star r) ys →
      matches (regexp.star r) (xs ++ ys)

inductive derivative : regexp → symbol → regexp → Prop
  | zero {s} : derivative regexp.zero s regexp.zero
  | one {s} : derivative regexp.one s regexp.zero
  | sym_eq {s} : derivative (regexp.sym s) s regexp.one
  | sym_ne {a s} : a ≠ s → derivative (regexp.sym a) s regexp.zero
  | alt {ra rb ra' rb' s} :
      derivative ra s ra' →
      derivative rb s rb' →
      derivative (regexp.alt ra rb) s (regexp.alt ra' rb')
  | cat_nil {ra rb ra' rb' s} :
      matches ra list.nil →
      derivative ra s ra' →
      derivative rb s rb' → 
      derivative (regexp.cat ra rb) s (regexp.alt (regexp.cat ra' rb) rb')
  | cat_non_nil {ra rb ra' s} :
      ¬matches ra list.nil →
      derivative ra s ra' →
      derivative (regexp.cat ra rb) s (regexp.cat ra' rb)
  | star (r r' s) :
      derivative r s r' →
      derivative (regexp.star r) s (regexp.cat r' (regexp.star r))

inductive derivative_chain : regexp → list symbol → regexp → Prop
  | nil {r} : derivative_chain r list.nil r
  | cons {r x r'} :
      derivative r x r' →
      ∀xs r'',
      derivative_chain r' xs r'' →
      derivative_chain r (x :: xs) r''

theorem zero_never_matches {xs} : ¬matches regexp.zero xs :=
begin
  intro h,
  cases h
end

theorem sym_not_nullable {s} : ¬matches (regexp.sym s) list.nil :=
begin
  intro h,
  cases h
end

theorem alt_not_match {ra rb xs} :
    ¬matches ra xs → ¬matches rb xs → ¬matches (regexp.alt ra rb) xs :=
begin
  intros na nb h,
  cases h,
  case matches.alt_l ha { apply (na ha) },
  case matches.alt_r hb { apply (nb hb) }
end

theorem nil_app {t} {xs:list t} :
    xs = list.nil ++ xs :=
begin
  refl
end

theorem append_eq_nil {t} {xs ys:list t} :
    list.nil = xs ++ ys → list.nil = xs ∧ list.nil = ys :=
begin
  intro h,
  induction xs,
  {
      split,
      refl,
      apply h
  },
  split ; cases h
end

theorem cat_matches_nil {ra rb} :
    matches (regexp.cat ra rb) list.nil →
    matches ra list.nil ∧ matches rb list.nil :=
begin
  intro h,
  cases h,
  case matches.cat xs ys ha hb hc {
    have hd : list.nil = xs ∧ list.nil = ys,
      from (append_eq_nil hc),
    cases hd with hx hy,
    rewrite ← hx at ha,
    rewrite ← hy at hb,
    split ; assumption
  }
end

theorem cat_left_no_match_nil {ra rb} :
    ¬matches ra list.nil → ¬matches (regexp.cat ra rb) list.nil :=
begin
  intros ha h,
  apply ha,
  apply (and.left (cat_matches_nil h)),
end

theorem cat_right_no_match_nil {ra rb} :
    ¬matches rb list.nil → ¬matches (regexp.cat ra rb) list.nil :=
begin
  intros ha h,
  apply ha,
  apply (and.right (cat_matches_nil h)),
end

inductive res (P:Prop) : Type
  | yes : P → res
  | no : ¬P → res

theorem or_both_false {P Q:Prop} : ¬P → ¬Q → ¬(P ∨ Q) :=
begin
  intros np nq h,
  cases h with p q,
  apply (np p),
  apply (nq q)
end

theorem and_left_false {P Q:Prop} : ¬P → ¬(P ∧ Q) :=
begin
  intros np h,
  cases h with p q,
  apply (np p)
end

theorem and_right_false {P Q:Prop} : ¬Q → ¬(P ∧ Q) :=
begin
  intros nq h,
  cases h with p q,
  apply (nq q)
end

def or_res {P Q:Prop} : res P → res Q → res (P ∨ Q)
  | (res.yes p) _ := res.yes (or.inl p)
  | _ (res.yes q) := res.yes (or.inr q)
  | (res.no np) (res.no nq) := res.no (or_both_false np nq)

def and_res {P Q:Prop} : res P → res Q → res (P ∧ Q)
  | (res.yes p) (res.yes q) := res.yes (and.intro p q)
  | (res.no np) _ := res.no (and_left_false np)
  | _ (res.no nq) := res.no (and_right_false nq)

def alt_res {ra rb:regexp} {xs} : res (matches ra xs) → res (matches rb xs) → res (matches (regexp.alt ra rb) xs)
  | (res.yes p) _ := res.yes (matches.alt_l p)
  | _ (res.yes q) := res.yes (matches.alt_r q)
  | (res.no np) (res.no nq) := res.no (alt_not_match np nq)

def cat_res {ra rb:regexp} : res (matches ra list.nil) → res (matches rb list.nil) → res (matches (regexp.cat ra rb) list.nil)
  | (res.yes p) (res.yes q) := res.yes (matches.cat p q nil_app)
  | (res.no np) _ := res.no (cat_left_no_match_nil np)
  | _ (res.no nq) := res.no (cat_right_no_match_nil nq)

def nullable : Π (r:regexp), res (matches r list.nil)
  | regexp.zero := res.no zero_never_matches
  | regexp.one := res.yes matches.one
  | (regexp.sym _) := res.no sym_not_nullable
  | (regexp.alt ra rb) := alt_res (nullable ra) (nullable rb)
  | (regexp.cat ra rb) := cat_res (nullable ra) (nullable rb)
  | (regexp.star _) := res.yes matches.star_nil

def diff : regexp → symbol → regexp
  | regexp.zero _ := regexp.zero
  | regexp.one _ := regexp.zero
  | (regexp.sym a) b :=
        match nat.decidable_eq a b with
        | is_true _ := regexp.one
        | is_false _ := regexp.zero
        end
  | (regexp.alt ra rb) s := regexp.alt (diff ra s) (diff rb s)
  | (regexp.cat ra rb) s :=
        match nullable ra with
        | res.yes _ := (regexp.alt (regexp.cat (diff ra s) rb) (diff rb s))
        | res.no _ := (regexp.cat (diff ra s) rb)
        end
  | (regexp.star r) s := regexp.cat (diff r s) (regexp.star r)

theorem diff_correct {r x} :
    derivative r x (diff r x) :=
begin
  induction r ; unfold diff,
  case regexp.zero {
    constructor
  },
  case regexp.one {
    constructor
  },
  case regexp.sym a {
    cases (nat.decidable_eq a x) with hneq heq,
    apply derivative.sym_ne ; assumption,
    unfold diff._match_1,
    rewrite heq,
    apply derivative.sym_eq
  },
  case regexp.alt ra rb {
    apply derivative.alt ; assumption
  },
  case regexp.cat ra rb {
    cases (nullable ra) with hn hnn,
    {
      apply derivative.cat_nil,
      assumption,
      unfold diff,
      assumption,
      unfold diff,
      assumption
    },
    {
      apply derivative.cat_non_nil,
      assumption,
      unfold diff,
      assumption
    }
  },
  case regexp.star {
    apply derivative.star,
    assumption
  }
end

theorem derivative_matches {r x r'} :
    derivative r x r' →
    ∀xs,
    matches r' xs →
    matches r (x :: xs) :=
begin
  intro hd,
  induction hd ; intros xs hm ; try { cases hm ; fail_if_success { skip } },
  case derivative.sym_eq {
    cases hm,
    apply matches.sym
  },
  case derivative.alt {
    cases hm,
    {
      apply matches.alt_l,
      apply ih_1,
      assumption
    },
    {
      apply matches.alt_r,
      apply ih_2,
      assumption
    }
  },
  case derivative.cat_nil ra rb ra' rb' s hn hda hdb {
    cases hm,
    {
      cases a,
      rewrite a_3 at *,
      rewrite ← list.cons_append,
      apply matches.cat,
      apply ih_1,
      apply a_1,
      apply a_2,
      refl
    },
    {
      rewrite ← (list.append_nil (s :: xs)),
      apply matches.cat,
      apply hn,
      apply ih_2,
      apply a,
      rewrite list.nil_append,
      rewrite list.append_nil
    }
  },
  case derivative.cat_non_nil ra rb ra' s hn hda {
    cases hm,
    rewrite a_2 at *,
    rewrite ← list.cons_append,
    apply matches.cat,
    apply ih_1,
    apply a,
    apply a_1,
    refl
  },
  case derivative.star r r' s
  {
    cases hm,
    rewrite a_3 at *,
    rewrite ← list.cons_append,
    apply matches.star_cons,
    {
      apply ih_1,
      assumption
    },
    assumption
  }
end

def diff_list : list symbol → regexp → regexp
  | list.nil r := r
  | (x :: xs) r := diff_list xs (diff r x)

theorem diff_list_correct {xs} :
    ∀r r', diff_list xs r = r' →
    derivative_chain r xs r' :=
begin
  induction xs ; intros r r' h,
  case list.nil {
    unfold diff_list at h,
    rewrite h,
    constructor
  },
  case list.cons x xs' {
    unfold diff_list at h,
    rewrite ← h,
    apply derivative_chain.cons,
    apply diff_correct,
    apply ih_1,
    refl
  }
end

theorem derivative_chain_matches {r xs r'} :
    derivative_chain r xs r' →
    matches r' list.nil →
    matches r xs :=
begin
  intros h,
  induction h ; intro hm,
  assumption,
  case derivative_chain.cons r x r' r'' xs' hd hc {
      apply derivative_matches,
      apply hd,
      apply ih_1,
      apply hm
  }
end

theorem diff_list_matches {r xs p} :
  nullable (diff_list xs r) = res.yes p →
  matches r xs :=
begin
  intro hn,
  apply (@derivative_chain_matches r xs (diff_list xs r)),
  apply diff_list_correct,
  refl,
  apply p
end

end regexp

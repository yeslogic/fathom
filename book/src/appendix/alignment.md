# Size and alignment of dependently typed data

Engineers have devised a vast number of data formats for
a wide range of applications,
and are devising new ones all the time.
Data Description Languages (DDLs) are formal languages designed for
writing specifications of data formats;
formalizing a spec in this way can help with
verifying that an application correctly implements the spec,
as well as validating that the spec really says what the author
intended it to say.

Typically, DDLs use dependent types to give a precise
characterisation of what is considered valid data.
For example, it is common for a format to contain a field specifying
the size of the data, followed by an array of the data itself.
In this case the _type_ of the array depends on
the _value_ of the field preceding it,
hence the data is dependently typed.
In contrast, most programming languages would leave the array size
out of the type, and it would either be left to programmer discipline
to ensure that offsets into the data stream are correctly aligned
and that dynamic bounds checks are performed when needed,
or else such checks would always be performed.

So what can we say, statically, about the size of terms of a given type?
Such information is useful not just to cut down on parsing time
by avoiding the need for unnecessary checks,
but also as a guide for authors who are writing a new spec
or formulating an existing spec in a DDL.

In this article, we present a small language of dependently typed terms
representing a simple form of DDL,
which includes integers, arrays, union types, recursive types
and, of course, dependent sums.
Although the size of a term cannot in general
be statically determined from its type,
we show how to calculate an approximation of the set of possible sizes
for any type.
Since this is only an approximation,
not every value in the set will be the size of some term,
however it is still true that every term has a size in the set.
Hence, if all of the values in the set are aligned in a particular way,
we can conclude that the size of the term will always be aligned that way
as well.

## Preliminaries (optional)

This section covers some background material that we use in the analysis.
You can skip this part if it is already familiar to you.

### Polynomials

A (multivariate) polynomial is an expression that is built up using:

- integer constants
- variables
- binary operations <code>*</code> and <code>+</code>

Any polynomial can be written in a simplified form as follows:
\\[
    k_1x_{11}^{i_{11}}x_{12}^{i_{12}}\ldots +
    k_2x_{21}^{i_{21}}x_{22}^{i_{22}}\ldots +
    \ldots
\\]
That is, a sum of terms each of which is an integer,
called the coefficient,
multiplied by a set of variables raised to some power.

Two terms in a polynomial are "like" when they contain the same variables
raised to the same powers;
such terms can be combined by adding the coefficients.
In the sequel, we will assume that all polynomials are simplified as
much as possible.
That is, they are in the above form and all like terms have been combined.

You're probably familiar with the formula
<code>
<pre>
    (a // m) * m + (a % m) = a
</pre>
</code>
where <code>//</code> is integer division and <code>%</code> is "mod".
This works for positive integers,
and if the operations are defined carefully
it can work for negative integers as well, and even zero.

Polynomials have a long division operation analogous to integer division,
and a similar formula to the above holds for it.
We won't need the full definition of polynomial long division,
so that won't be covered here, but we will need to use one special case,
namely, dividing by a polynomial consisting of a single variable.

The definition is very simple.
Assuming the polynomial is <code>p</code> and the variable is <code>x</code>,
then:

- <code>(p // x)</code> is the polynomial consisting of
  each term of <code>p</code> that contains <code>x</code>,
  with the power of <code>x</code> reduced by 1.
  (Note that anything raised to the power 0 is 1,
  and is therefore from the term.)
- <code>(p % x)</code> is the polynomial consisting of
  each term of <code>p</code> that does not contain <code>x</code>.

It is easy to check that
<code>
<pre>
    (p // x) * x + (p % x) = p
</pre>
</code>
as claimed.

### Greatest common divisor

We say that \\(a\\) divides \\(b\\),
written \\(a \vert b\\),
if there exists some \\(k ∈ \mathbb{Z}\\) such that \\(ka = b\\).
Under this definition,
the following are true for all \\(x\\), \\(y \in \mathbb{Z}\\):

- \\(x \vert 0\\)
- \\(1 \vert x\\)
- \\(\text{if}\ x \vert y \ \text{then}\ x \vert {-y}\\)
- \\(\text{if}\ x \vert y \ \text{then}\ {-x} \vert y\\)

In the first case, \\(k = 0\\);
in the second case, \\(k = x\\);
in the last two cases, \\(k\\) is negated.
Note that this operator is sometimes defined to exclude the first case
(that is, require that \\(k \neq 0\\)),
but for the purposes of our analysis it is necessary to include this case.

The greatest common divisor (gcd) of a set \\(S\\) is defined as
the least upper bound of the set of integers \\(x\\) that satisfy
\\[
     \forall s \in S . x \vert s
\\]
This set does not have a least upper bound if \\(S\\) is empty,
or only contains 0;
in these cases we define the gcd itself to be 0.
Although this is not strictly justified by the above definition of "divides",
it saves us dealing with a lot of special cases in the analysis below.

In a similar spirit, we extend the gcd function to polynomials,
defining it as the gcd of the coefficients of the given polynomial.
(Here is where it is important that we simplify the polynomial
as much as possible,
since we may get a weaker result
if the polynomial is not fully simplified.)

As an example, the gcd of \\(6x^2 + 12xy - 4y^2\\) is \\(2\\).

## A simple dependently-typed langauge

### Terms

Terms in our language will play the role of the data that is being described.
For the purposes of this article we will keep it minimal
and only include integer types of various sizes,
but it is not hard to see how to add a richer set of base types.

Terms conform to the following grammar:

\\[
    \\begin{array}{rrl}
    t   & ::= & κ_{\text{bits}} \\\\
        &   | & () \\\\
        &   | & (t_{1}, t_{2}) \\\\
        &   | & [t_{1}, \ldots , t_{n}] \\\\
    \\end{array}
\\]

These, respectively, are integer literals (with a size annotation),
the empty tuple, pairs, and arrays.
The number of elements in an array may be zero or more.

The following is an example of a valid term:

\\[
     (3_{16}, [3_{8}, 4_{8}, 9_{8}])
\\]

This is a pair containing a 16-bit integer
followed by an array of three 8-bit integers.

We define a size function on terms as follows:

\\[
    \\begin{array}{rlrl}
        \text{size}(& κ_{b} &)                  & = b \\\\
        \text{size}(& () &)                     & = 0 \\\\
        \text{size}(& (t_{1}, t_{2}) &)         & = \text{size}(t_{1}) + \text{size}(t_{2}) \\\\
        \text{size}(& [t_{1}, \ldots, t_{n}] &) & = \text{size}(t_{1}) + \dots + \text{size}(t_{n}) \\\\
    \\end{array}
\\]

In the next section we will give typing rules,
and later we will give rules for approximating the set of possible sizes.
The main theorem of this article will be that, for any typing derivation,
the size of the term according to the size function
will be in the set of possible sizes that we calculate for that type.

### Types

Types conform to the following grammar:
\\[
    \\begin{array}{rrl}
    \tau & ::= & i(b, p, a) \\\\
         &   | & \text{unit} \\\\
         &   | & \Sigma x:τ_{1}.\tau_{2} \\\\
         &   | & \tau[e] \\\\
         &   | & \tau_{1} \cup \tau_{2} \\\\
         &   | & \alpha \\\\
         &   | & \mu \alpha.\tau \\\\
    \\end{array}
\\]
A type of the form \\(i(b, p, a)\\)
is a signed integer type with size \\(b\\).
The other two parameters tell us something about which integers are included,
namely, integers from the set \\(\lbrace p + ka \vert k \in \mathbb{Z} \rbrace\\).
Thus, if \\(a\\) is 1 then this includes all integers in the size range,
and if \\(a\\) is 0 then the integer is a constant equal to \\(p\\).
If \\(p\\) is 0, then the values will be multiples of \\(a\\),
hence this tells us if they have any alignment.

\\(\text{unit}\\) is the type of the empty tuple,
and \\(\Sigma\\)-types are used for pairs.
\\(\tau[e]\\) is the type of an array of \\(e\\) elements,
each of type \\(\tau\\).
\\(e\\) stands for an expression, which for the moment we will assume is
a polynomial in the \\(\Sigma\\)-quantified variables,
but we will discuss a broader range of expressions later on.

\\(\tau_{1} \cup \tau_{2}\\) is the undiscriminated union of two types.
In practice, DDLs usually offer some mechanism to discriminate
between the two branches of a union,
such as conditional types or simply trying each branch in order.
For our present purposes the exact mechanism is not important,
so we will avoid making any assumptions about how it will work;
it turns out not to affect the results we are able to get.

\\(\alpha\\) is a type variable,
which might be a μ-quantified type
or else a type that is already defined in the environment.
\\(\mu \alpha.\tau\\) is a recursive type;
that is, the type that results by substituting the μ-type itself
for every occurrence of \\(\alpha\\) in \\(\tau\\).

The preceding paragraphs are formalized in the following typing rules.

<div class="proof-rules">
<div class="proof-rule">
<code>
<pre>
     ---------------------  (CONST)
     Γ;Δ  ⊢  κ<sub><var>b</var></sub> : i(<var>b</var>,κ,0)
</pre>
</code>
</div>
<div class="proof-rule">
<code>
<pre>
     -----------------  (UNIT)
     Γ;Δ  ⊢  () : unit
</pre>
</code>
</div>
<div class="proof-rule">
<code>
<pre>
     Γ;Δ  ⊢  t<sub>1</sub> : τ<sub>1</sub>    Γ,x=t<sub>1</sub>;Δ  ⊢  t<sub>2</sub> : τ<sub>2</sub>
     --------------------------------------  (PAIR)
           Γ;Δ  ⊢  (t<sub>1</sub>, t<sub>2</sub>) : Σx:τ<sub>1</sub>.τ<sub>2</sub>
</pre>
</code>
</div>
<div class="proof-rule">
<code>
<pre>
     Γ;Δ  ⊢  t<sub>i</sub> : τ  (for i = 1,...,n)    Γ ⊨ e=n
     ---------------------------------------------  (ARRAY)
              Γ;Δ  ⊢  [t<sub>1</sub>, ..., t<sub>n</sub>] : τ[e]
</pre>
</code>
</div>
<div class="proof-rule">
<code>
<pre>
       Γ;Δ  ⊢  t : τ<sub>1</sub>
     -------------------  (INL)
     Γ;Δ  ⊢  t : τ<sub>1</sub> ∪ τ<sub>2</sub>
</pre>
</code>
</div>
<div class="proof-rule">
<code>
<pre>
       Γ;Δ  ⊢  t : τ<sub>2</sub>
     -------------------  (INR)
     Γ;Δ  ⊢  t : τ<sub>1</sub> ∪ τ<sub>2</sub>
</pre>
</code>
</div>
<div class="proof-rule">
<code>
<pre>
     Γ;Δ  ⊢  t : Δ(α)
     ----------------  (VAR)
      Γ;Δ  ⊢  t : α
</pre>
</code>
</div>
<div class="proof-rule">
<code>
<pre>
     Γ;Δ,α=μα.τ  ⊢  t : τ
     --------------------  (REC)
       Γ;Δ  ⊢  t : μα.τ
</pre>
</code>
</div>
</div>
Here, the environment <code>Γ;Δ</code> maps Σ-quantified variables
to terms and μ-quantified variables to types.

## Alignment

### Notation

We introduce some notation that will simplify the following material.

The function <code>M</code> is defined as
<code>
<pre>
     M(<var>p</var>,<var>a</var>)  =  { <var>p</var> + k<var>a</var> | k ∈ ℤ }
</pre>
</code>
Recall that this is the set of allowed integers for the type
<code>i(<var>b</var>,<var>p</var>,<var>a</var>)</code>.

We will use the notation
<code>
<pre>
     Γ;Δ  ⊢  τ ⊑ M(<var>p</var>,<var>a</var>)
</pre>
</code>
to mean that every term of type <code>τ</code> in environment <code>Γ;Δ</code>
has a size that is an element of <code>M(<var>p</var>,<var>a</var>)</code>.

For the alignment analysis <code>Γ</code> is a _type_ environment,
which gives the types of Σ-quantified variables,
rather than the values as we did previously.
<code>Δ</code> is an environment
containing statements of the form <code>α ⊑ M(p,a)</code>
for type variables <code>α</code>.

### Rules

<div class="proof-rules">
<div class="proof-rule">
<code>
<pre>
     -----------------------  (CONST)
     Γ;Δ ⊢ i(<var>b</var>,<var>p</var>,<var>a</var>) ⊑ M(<var>b</var>,0)
</pre>
</code>
</div>
<div class="proof-rule">
<code>
<pre>
     -------------------  (UNIT)
     Γ;Δ ⊢ unit ⊑ M(0,0)
</pre>
</code>
</div>
<div class="proof-rule">
<code>
<pre>
       Γ;Δ ⊢ τ<sub>1</sub> ⊑ M(p<sub>1</sub>,a<sub>1</sub>)      Γ,x:τ<sub>1</sub>;Δ ⊢ τ<sub>2</sub> ⊑ M(p<sub>2</sub>,a<sub>2</sub>)
     ----------------------------------------------------  (PAIR)
     Γ;Δ ⊢ Σx:τ<sub>1</sub>.τ<sub>2</sub> ⊑ M(p<sub>1</sub>+(p<sub>2</sub> % x), gcd((p<sub>2</sub> // x),a<sub>1</sub>,a<sub>2</sub>))
</pre>
</code>
</div>
<div class="proof-rule">
<code>
<pre>
     Γ;Δ ⊢ τ ⊑ M(p<sub>τ</sub>,0)    Γ ⊢ e ⊑ M(p<sub>e</sub>,a<sub>e</sub>)
     -------------------------------------  (ARRAY)
        Γ;Δ ⊢ τ[e] ⊑ M(p<sub>τ</sub>p<sub>e</sub>, a<sub>e</sub>gcd(p<sub>τ</sub>))
</pre>
</code>
</div>
<div class="proof-rule">
<code>
<pre>
     Γ;Δ ⊢ τ<sub>1</sub> ⊑ M(p<sub>1</sub>,a<sub>1</sub>)    Γ;Δ ⊢ τ<sub>2</sub> ⊑ M(p<sub>2</sub>,a<sub>2</sub>)
     -----------------------------------------  (UNION)
      Γ;Δ ⊢ τ<sub>1</sub> ∪ τ<sub>2</sub> ⊑ M(p<sub>1</sub>, gcd((p<sub>2</sub>-p<sub>1</sub>),a<sub>1</sub>,a<sub>2</sub>))
</pre>
</code>
</div>
<div class="proof-rule">
<code>
<pre>
     --------------  (VAR)
     Γ;Δ ⊢ α ⊑ Δ(α)
</pre>
</code>
</div>
<div class="proof-rule">
<code>
<pre>
     Γ;Δ,α ⊑ M(0,0) ⊢ τ ⊑ M(p,a)
     ---------------------------  (REC)
     Γ;Δ ⊢ μα.τ ⊑ M(0,gcd(p,a))
</pre>
</code>
</div>
</div>

Notes:
<ul>
<li>
The CONST rule uses the <var>b</var> parameter,
not the <var>p</var> or <var>a</var> parameters.
That is because we are analyzing the _size_ of the integer,
not its value.

<li>
In the ARRAY rule, the <var>a</var> value for the element type must be zero.
This is equivalent to saying that we know all elements in the array
will have the same size.
This is necessary in order for us to be able to multiply the <var>p</var>
values to determine the overall size,
rather than summing over the elements as we did earlier.

<li>
Also in the array rule,
we require a judgement of the form <code>Γ ⊢ e ⊑ M(p<sub>e</sub>,a<sub>e</sub>)</code>.
For this judgement we can use the following rule
<code>
<pre>
     --------------  (EXPR)
     Γ ⊢ e ⊑ M(e,0)
</pre>
</code>
since we are already assuming that <code>e</code> is itself a polynomial.
Later we will show how to handle arbitrary expressions.

<li>
We don't actually make use of the <var>p</var> and <var>a</var> parameters
of the integer type.
For this we would need a modified version of the PAIR rule.
TBD.
</ul>

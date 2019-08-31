# SOL
Haskell implementation on John Mitchell and Gordon Plotkin's [SOL](http://theory.stanford.edu/~jcm/papers/mitch-plotkin-88.pdf). It has the features of System F but with existential types made explicit.

We have added `Nat`ural numbers, `Bool`eans, records, product, and sum (disjoint union) types to make this calculus slightly more interesting as Mitchell and Plotkin do. These types are not necessary for SOL, and [others](https://www.cs.cornell.edu/courses/cs4110/2018fa/lectures/lecture26.pdf) have presented existentials with just STLC, however we use this presentation to make clear their relation with parametricity.

This strongly normalising calculus makes it easier to see how abstract data types, objects, and (simplistic) modules can arise as a special case of parametric polymorphism.

## Prerequisites
You need [Haskell](https://www.haskell.org/), this compiles with GHC 8.2.2 at least (Stack resolver: lts-11.0).

## To Build & Run

You can use cabal to build and run this, see this [README](../README.md), alternatively you can use vanilla ghc to build:

To compile and run do:
`ghc -O2 -o sol Main`
then run `./sol`

Alternatively to use the GHCi Interpreter do:
`ghci Main`
then type `main`

In either case you get something like the following:
```
Welcome to the SOL REPL
Type some terms or press Enter to leave.
>
```

Note: When run in GHCi, you don't have the luxury of escaped characters, backspace, delete etc...
Compile it using GHC if you need this.

## Examples 
Where you can then have some fun, try these examples:
- `LX.\x:X.x` where `L` stands for second-order abstraction. See if you can see why `\x:X.x` won't type check.
- `(LX. \x:X.x) [PX.(X->X)->X->X]` the identity function for Church natural numbers. 
- `LX.\f:X->X.\x:X.x` this is _zero_ in Church numeral format.
- `2` Nats are supported.
- `if ((λb:Bool.b) true) then 0 else 1` booleans are supported.
- `LX.{f=\x:X.x, u=2}` This is a record with a universally quantified function and a constant. 
- `LB.\r:{b:B}.r.b` This is a function that takes a record and accesses field `b`.
- `case (inl 1: Nat + Bool) (\x:Nat.x) (\b:Bool.1)`
- `λn:∀X.(X->X)->X->X.ΛY.λf:Y->Y.λy:Y.f (n [Y] f y)` this is _succ_ in Church Numeral format.
- `fst (1,2)`, `snd (1,2)` are left and right projections on tuples.
- `pack {Nat, 4} as EX.X` existential quantification where `E` stands for 'there exists'.
- `pack {Nat, {init=0, set=\x:Nat.x}} as EX.{init:X, set:X->X}` packs the record up as an existentially quantified type `X` with operations `init` and `set`.
- `unpack {X,body} = foo in body.f` is the means to import an existentially quantified `foo` as variable `body` and substitute instances of the quantified type `X` in the body of the unpack statement. In this case `body` is a record that contains a field `f`.


Note: `∀` is the second-order product type, `∃` is existential quantification, `Λ` is second-order abstraction and `[X]` is the type variable `X`. Alternatively typed as `P`, `E` `L`, and `[X]` respectively.

The parser is smart enough to recognise λ, ∀ , ∃, and Λ; so you can copy and paste from the output:
```
Welcome to the System F REPL
Type some terms or press Enter to leave.
>   LX.\x:X.x
=   ΛX.λx:X.x
>   ΛX.λx:X.x
=   ΛX.λx:X.x
```
`>` denotes the REPL waiting for input, `=` means no reductions occurred (it's the same term), `~>` denotes one reduction, and `~>*` denotes 0 or more reductions (although in practice this is 1 or more due to `=`).

There is also a reduction tracer, which should print each reduction step. prefix any string with `'` in order to see the reductions:
```
>   '(λn:∀X.(X->X)->X->X.ΛY.λf:Y->Y.λy:Y.f (n [Y] f y)) (LX.\f:X->X.\x:X.x)
~>  ΛY.λf:Y->Y.λy:Y.f ((ΛX.λf:X->X.λx:X.x) [Y] f y)
~>  ΛY.λf:Y->Y.λy:Y.f ((λf:Y->Y.λx:Y.x) f y)
~>  ΛY.λf:Y->Y.λy:Y.f ((λx:Y.x) y)
~>  ΛY.λf:Y->Y.λy:Y.f y
```
Note: this is succ zero (or one) in Church numeral format

There is also a typing mechanism, which should display the type or fail as usual.
```
>   tpack {Nat, {init=0, set=\x:Nat.x}} as EX.{init:X, set:X->X}
∃X.{init:X, set:X->X}
>   tLX.\x:X. x
∀X.X->X
>   unpack {B,b} = (pack {Bool, \b:Bool.b} as EX.X->X) in b true
Cannot Type Term: unpack {"b", "B"} = pack {Bool, λb:Bool.b} as ∃X.X->X in b true
```
where `∀Y.(Y->Y)->Y->Y` is equivalent to the System F/SOL type for `Nats`.

Note: if you provide a non-normalizing term, the type checker will fail and reduction will not occur.

You can save terms for the life of the program with a `let` expression. Any time a saved variable appears in a term, it will be substituted for the saved term:
```
>   let foo = pack {Nat, {init=0, set=\x:Nat.x}} as EX.{init:X, set:X->X}
Saved term: pack {Nat, {init=0, set=λx:Nat.x}} as ∃X.{init:X, set:X->X}
>   unpack {X,body} = foo in body
~>* {init=0, set=λx:Nat.x}
>   unpack {X,body} = foo in body.init
~>* 0
```

Note: Consequently `let` and `=` are keywords, and so you cannot name variables with these. Additionally `L`, `[`, `]`, and `P`, `E`, `pack`, `unpack`, `true`, `false`, `fst`, `snd`, `inl`, `inr`, `case`, `*`, `+`, `Nat`, `Bool`, `{`, `}`, `as`, `(`, `)`, `if`, `then`, `else` and `in` are keywords in SOL.

Additionally we have type level `lett` statements that allow you to define and use types:
```
>   lett REGISTER = EX.{init:X, set:X->X}
Saved type: ∃X.{init:X, set:X->X}
>   let reg = pack {Nat, {init=0, set=λx:Nat.x}} as REGISTER
Saved term: pack {Nat, {init=0, set=λx:Nat.x}} as ∃X.{init:X, set:X->X}
>   unpack {R,register} = reg in register.init
~>* 0
```
This makes it easier to define both terms and types, but does not allow type level application (See Omega). `lett` is also a keyword.

## Syntax 

We base the language on the BNF for the typed calculus:

<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}&space;\mathbf{\tau}&&space;::=&space;&&space;\lambda&space;\mathbf{\upsilon}{\tt&space;:}\sigma&space;.&space;\mathbf{\tau}\\&space;&&space;|&space;&&space;\tau\,&space;\tau\\&space;&&space;|&space;&&space;\upsilon&space;\\&space;&&space;|&space;&&space;\Lambda&space;\mu.\tau\\&space;&|&&space;[\sigma]&space;&&\\&|&&space;\tt{inl}\,\tau:\sigma\\&space;&|&&space;\tt{inr}\,\tau:\sigma\\&space;&|&&space;\tt{case}\,\tau\,\tau\,\tau\\&space;&|&&space;(\tau,&space;\tau)\\&space;&|&&space;\pi_{1}/\tt{fst}\,\tau\\&space;&|&&space;\pi_{2}/\tt{snd}\,\tau\\\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}&space;\mathbf{\tau}&&space;::=&space;&&space;\lambda&space;\mathbf{\upsilon}{\tt&space;:}\sigma&space;.&space;\mathbf{\tau}\\&space;&&space;|&space;&&space;\tau\,&space;\tau\\&space;&&space;|&space;&&space;\upsilon&space;\\&space;&&space;|&space;&&space;\Lambda&space;\mu.\tau\\&space;&|&&space;[\sigma]&space;&&\\&|&&space;\tt{inl}\,\tau:\sigma\\&space;&|&&space;\tt{inr}\,\tau:\sigma\\&space;&|&&space;\tt{case}\,\tau\,\tau\,\tau\\&space;&|&&space;(\tau,&space;\tau)\\&space;&|&&space;\pi_{1}/\tt{fst}\,\tau\\&space;&|&&space;\pi_{2}/\tt{snd}\,\tau\\\end{matrix}" title="\begin{matrix} \mathbf{\tau}& ::= & \lambda \mathbf{\upsilon}{\tt :}\sigma . \mathbf{\tau}\\ & | & \tau\, \tau\\ & | & \upsilon \\ & | & \Lambda \mu.\tau\\ &|& [\sigma] &&\\&|& \tt{inl}\,\tau:\sigma\\ &|& \tt{inr}\,\tau:\sigma\\ &|& \tt{case}\,\tau\,\tau\,\tau\\ &|& (\tau, \tau)\\ &|& \pi_{1}/\tt{fst}\,\tau\\ &|& \pi_{2}/\tt{snd}\,\tau\\\end{matrix}" /></a>
<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}&space;&|&\tt{if}\,&space;\tau\,&space;\tt{then}\,&space;\tau\,&space;\tt{else}\,&space;\tau\\&|&&space;\tt{true}&space;&&\\&space;&|&&space;\tt{false}&space;&&\\&space;&|&&space;\eta&space;&&\\&space;&|&&space;\{\upsilon=&space;\tau,...\}&space;\\&space;&|&\tau&space;.&space;\upsilon\\&space;&|&&space;\tt{pack}\,\{\sigma,\tau&space;\}\,&space;\tt{as}\,&space;\sigma&space;\\&space;&|&&space;\tt{unpack}\,\{\mu,\upsilon&space;\}\,=\tau\,&space;as\,&space;\tau&space;\\\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}&space;&|&\tt{if}\,&space;\tau\,&space;\tt{then}\,&space;\tau\,&space;\tt{else}\,&space;\tau\\&|&&space;\tt{true}&space;&&\\&space;&|&&space;\tt{false}&space;&&\\&space;&|&&space;\eta&space;&&\\&space;&|&&space;\{\upsilon=&space;\tau,...\}&space;\\&space;&|&\tau&space;.&space;\upsilon\\&space;&|&&space;\tt{pack}\,\{\sigma,\tau&space;\}\,&space;\tt{as}\,&space;\sigma&space;\\&space;&|&&space;\tt{unpack}\,\{\mu,\upsilon&space;\}\,=\tau\,&space;as\,&space;\tau&space;\\\end{matrix}" title="\begin{matrix} &|&\tt{if}\, \tau\, \tt{then}\, \tau\, \tt{else}\, \tau\\&|& \tt{true} &&\\ &|& \tt{false} &&\\ &|& \eta &&\\ &|& \{\upsilon= \tau,...\} \\ &|&\tau . \upsilon\\ &|& \tt{pack}\,\{\sigma,\tau \}\, \tt{as}\, \sigma \\ &|& \tt{unpack}\,\{\mu,\upsilon \}\,=\tau\, as\, \tau \\\end{matrix}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}&space;\sigma&space;&&space;::=&space;&&space;{\tt&space;\mu&space;}\\&space;&&space;|&space;&&space;\sigma&space;\rightarrow&space;\sigma\\&space;&|&&space;\forall&space;\mu&space;.&space;\sigma\\&space;&|&\exists&space;\mu&space;.&space;\sigma\\&space;&|&&space;Nat\\&space;&|&Bool\\&|&\{\mathbf{\upsilon}\tt{:}\sigma,...\}\\&space;&|&&space;\sigma&space;\times&space;\sigma\\&space;&|&&space;\sigma&space;&plus;&space;\sigma\\\\&space;\mu&space;&&space;::=&space;&&space;\tt{A}&space;|&space;\tt{B}&space;|&space;\tt{C}&space;|&space;...&space;|&space;\tt{AA}&space;|&space;...&space;\\&space;\eta&space;&&space;::=&space;&&space;\tt{0}&space;|&space;\tt{1}&space;|&space;\tt{3}&space;|&space;...&space;|&space;\tt{42}&space;|&space;...&space;\\\upsilon&space;&&space;::=&space;&&space;\tt{a}&space;|&space;\tt{b}&space;|&space;\tt{c}&space;|&space;...&space;|&space;\tt{aa}&space;|&space;...&space;\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}&space;\sigma&space;&&space;::=&space;&&space;{\tt&space;\mu&space;}\\&space;&&space;|&space;&&space;\sigma&space;\rightarrow&space;\sigma\\&space;&|&&space;\forall&space;\mu&space;.&space;\sigma\\&space;&|&\exists&space;\mu&space;.&space;\sigma\\&space;&|&&space;Nat\\&space;&|&Bool\\&|&\{\mathbf{\upsilon}\tt{:}\sigma,...\}\\&space;&|&&space;\sigma&space;\times&space;\sigma\\&space;&|&&space;\sigma&space;&plus;&space;\sigma\\\\&space;\mu&space;&&space;::=&space;&&space;\tt{A}&space;|&space;\tt{B}&space;|&space;\tt{C}&space;|&space;...&space;|&space;\tt{AA}&space;|&space;...&space;\\&space;\eta&space;&&space;::=&space;&&space;\tt{0}&space;|&space;\tt{1}&space;|&space;\tt{3}&space;|&space;...&space;|&space;\tt{42}&space;|&space;...&space;\\\upsilon&space;&&space;::=&space;&&space;\tt{a}&space;|&space;\tt{b}&space;|&space;\tt{c}&space;|&space;...&space;|&space;\tt{aa}&space;|&space;...&space;\end{matrix}" title="\begin{matrix} \sigma & ::= & {\tt \mu }\\ & | & \sigma \rightarrow \sigma\\ &|& \forall \mu . \sigma\\ &|&\exists \mu . \sigma\\ &|& Nat\\ &|&Bool\\&|&\{\mathbf{\upsilon}\tt{:}\sigma,...\}\\ &|& \sigma \times \sigma\\ &|& \sigma + \sigma\\\\ \mu & ::= & \tt{A} | \tt{B} | \tt{C} | ... | \tt{AA} | ... \\ \eta & ::= & \tt{0} | \tt{1} | \tt{3} | ... | \tt{42} | ... \\\upsilon & ::= & \tt{a} | \tt{b} | \tt{c} | ... | \tt{aa} | ... \end{matrix}" /></a>

However we adopt standard bracketing conventions to eliminate ambiguity in the parser. Concretely, the parser implements the non-ambiguous grammar for terms as follows:

<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}&space;&&\\&space;\mathbf{\tau}&&space;::=&space;&&space;\lambda&space;\mathbf{\upsilon}\tt{:}\sigma&space;.&space;\mathbf{\tau}\\&space;&|&&space;\Lambda\mu.\tau&space;\\&|&&space;\tt{pack}\{&space;\sigma,\tau\}\,\tt{as}\,\sigma\\&|&\tt{unpack}&space;\{\mu,\upsilon\}&space;=&space;\tau\,&space;\tt{in}\,\tau\\&|&\alpha\\&space;&&\\&space;\alpha&space;&&space;::=&space;&&space;\beta&space;\\&space;&|&space;&\mathbf{\alpha\,&space;\tt{space}\,&space;\beta}&space;\\&space;&|&&space;\[&space;\sigma\]&space;\\\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}&space;&&\\&space;\mathbf{\tau}&&space;::=&space;&&space;\lambda&space;\mathbf{\upsilon}\tt{:}\sigma&space;.&space;\mathbf{\tau}\\&space;&|&&space;\Lambda\mu.\tau&space;\\&|&&space;\tt{pack}\{&space;\sigma,\tau\}\,\tt{as}\,\sigma\\&|&\tt{unpack}&space;\{\mu,\upsilon\}&space;=&space;\tau\,&space;\tt{in}\,\tau\\&|&\alpha\\&space;&&\\&space;\alpha&space;&&space;::=&space;&&space;\beta&space;\\&space;&|&space;&\mathbf{\alpha\,&space;\tt{space}\,&space;\beta}&space;\\&space;&|&&space;\[&space;\sigma\]&space;\\\end{matrix}" title="\begin{matrix} &&\\ \mathbf{\tau}& ::= & \lambda \mathbf{\upsilon}\tt{:}\sigma . \mathbf{\tau}\\ &|& \Lambda\mu.\tau \\&|& \tt{pack}\{ \sigma,\tau\}\,\tt{as}\,\sigma\\&|&\tt{unpack} \{\mu,\upsilon\} = \tau\, \tt{in}\,\tau\\&|&\alpha\\ &&\\ \alpha & ::= & \beta \\ &| &\mathbf{\alpha\, \tt{space}\, \beta} \\ &|& \[ \sigma\] \\\end{matrix}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}&space;\beta&space;&&space;::=&space;&&space;\tt{(}\tau&space;\tt{)}\\&space;&|&&space;\upsilon&space;\\&|&&space;\{\upsilon=&space;\tau,...\}&space;\\&space;&|&\tau&space;.&space;\upsilon\\&space;&|&b\\&|&\tt{if}\,&space;\beta\,&space;\tt{then}\,\beta\,\tt{else}\,\beta\\&|&\eta\\&|&&space;(\tau,\tau)\\&space;&|&&space;p\,\tau\\&space;&|&&space;inl\,\tau:\sigma\\&space;&|&&space;inr\,\tau:\sigma\\&space;&|&&space;case\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}&space;\beta&space;&&space;::=&space;&&space;\tt{(}\tau&space;\tt{)}\\&space;&|&&space;\upsilon&space;\\&|&&space;\{\upsilon=&space;\tau,...\}&space;\\&space;&|&\tau&space;.&space;\upsilon\\&space;&|&b\\&|&\tt{if}\,&space;\beta\,&space;\tt{then}\,\beta\,\tt{else}\,\beta\\&|&\eta\\&|&&space;(\tau,\tau)\\&space;&|&&space;p\,\tau\\&space;&|&&space;inl\,\tau:\sigma\\&space;&|&&space;inr\,\tau:\sigma\\&space;&|&&space;case\end{matrix}" title="\begin{matrix} \beta & ::= & \tt{(}\tau \tt{)}\\ &|& \upsilon \\&|& \{\upsilon= \tau,...\} \\ &|&\tau . \upsilon\\ &|&b\\&|&\tt{if}\, \beta\, \tt{then}\,\beta\,\tt{else}\,\beta\\&|&\eta\\&|& (\tau,\tau)\\ &|& p\,\tau\\ &|& inl\,\tau:\sigma\\ &|& inr\,\tau:\sigma\\ &|& case\end{matrix}" /></a>

and types:

<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}&space;\sigma&space;&&space;::=&space;&&space;\gamma&space;\\&space;&&space;|&space;&&space;\gamma&space;\tt{\rightarrow}&space;\sigma&space;\\&space;&|&&space;\forall\mu.\sigma\\&space;&|&\exists&space;\mu.\sigma\\&space;&&\\&space;\gamma&space;&&space;::=&&space;\delta&space;&plus;&space;\delta\\&space;&|&\delta\\&&\\&space;\delta&space;&::=&&space;\rho&space;\times&space;\rho\\&space;&|&&space;\rho\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}&space;\sigma&space;&&space;::=&space;&&space;\gamma&space;\\&space;&&space;|&space;&&space;\gamma&space;\tt{\rightarrow}&space;\sigma&space;\\&space;&|&&space;\forall\mu.\sigma\\&space;&|&\exists&space;\mu.\sigma\\&space;&&\\&space;\gamma&space;&&space;::=&&space;\delta&space;&plus;&space;\delta\\&space;&|&\delta\\&&\\&space;\delta&space;&::=&&space;\rho&space;\times&space;\rho\\&space;&|&&space;\rho\end{matrix}" title="\begin{matrix} \sigma & ::= & \gamma \\ & | & \gamma \tt{\rightarrow} \sigma \\ &|& \forall\mu.\sigma\\ &|&\exists \mu.\sigma\\ &&\\ \gamma & ::=& \delta + \delta\\ &|&\delta\\&&\\ \delta &::=& \rho \times \rho\\ &|& \rho\end{matrix}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}&space;\rho&space;&&space;::=&&space;\tt{(}&space;\sigma&space;\tt{)}\\&space;&|&&space;\tt{Nat}\\&|&\tt{Bool}\\&&space;|&&space;\{\mathbf{\upsilon}\tt{:}\sigma,...\}&space;\\&space;&|&&space;\mu&space;\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}&space;\rho&space;&&space;::=&&space;\tt{(}&space;\sigma&space;\tt{)}\\&space;&|&&space;\tt{Nat}\\&|&\tt{Bool}\\&&space;|&&space;\{\mathbf{\upsilon}\tt{:}\sigma,...\}&space;\\&space;&|&&space;\mu&space;\end{matrix}" title="\begin{matrix} \rho & ::=& \tt{(} \sigma \tt{)}\\ &|& \tt{Nat}\\&|&\tt{Bool}\\& |& \{\mathbf{\upsilon}\tt{:}\sigma,...\} \\ &|& \mu \end{matrix}" /></a>

with term variables/primitives:

<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}&space;b&space;&&space;::=&space;&&space;\tt{true}&space;|&space;\tt{false}\\\upsilon&space;&&space;::=&space;&&space;\tt{a}&space;|&space;\tt{b}&space;|&space;\tt{c}&space;|&space;...&space;|&space;\tt{aa}&space;|&space;...\\&space;\eta&space;&&space;::=&space;&&space;\tt{0}&space;|&space;\tt{1}&space;|&space;\tt{3}&space;|&space;...&space;|&space;\tt{42}&space;|&space;...&space;\\&space;p&&space;::=&\pi_{1}\,|&space;\pi_{2}\,|\,\tt{fst}\,|\,\tt{snd}\\&space;\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}&space;b&space;&&space;::=&space;&&space;\tt{true}&space;|&space;\tt{false}\\\upsilon&space;&&space;::=&space;&&space;\tt{a}&space;|&space;\tt{b}&space;|&space;\tt{c}&space;|&space;...&space;|&space;\tt{aa}&space;|&space;...\\&space;\eta&space;&&space;::=&space;&&space;\tt{0}&space;|&space;\tt{1}&space;|&space;\tt{3}&space;|&space;...&space;|&space;\tt{42}&space;|&space;...&space;\\&space;p&&space;::=&\pi_{1}\,|&space;\pi_{2}\,|\,\tt{fst}\,|\,\tt{snd}\\&space;\end{matrix}" title="\begin{matrix} b & ::= & \tt{true} | \tt{false}\\\upsilon & ::= & \tt{a} | \tt{b} | \tt{c} | ... | \tt{aa} | ...\\ \eta & ::= & \tt{0} | \tt{1} | \tt{3} | ... | \tt{42} | ... \\ p& ::=&\pi_{1}\,| \pi_{2}\,|\,\tt{fst}\,|\,\tt{snd}\\ \end{matrix}" /></a>

and type variables:

<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}&space;\mu&space;&&space;::=&space;&&space;\tt{A}&space;|&space;\tt{B}&space;|&space;\tt{C}&space;|&space;...&space;|&space;\tt{AA}&space;|&space;...&space;\\\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}&space;\mu&space;&&space;::=&space;&&space;\tt{A}&space;|&space;\tt{B}&space;|&space;\tt{C}&space;|&space;...&space;|&space;\tt{AA}&space;|&space;...&space;\\\end{matrix}" title="\begin{matrix} \mu & ::= & \tt{A} | \tt{B} | \tt{C} | ... | \tt{AA} | ... \\\end{matrix}" /></a>

Some notes about the syntax:

- The above syntax only covers the core calculus, and not the repl extensions (such as let bindings above). The extensions are simply added on in the repl.
- Variables are strings (excluding numbers), as this is isomorphic to a whiteboard treatment and hence the most familiar.
- Natural numbers are positive of the form `1`, `2`, `3` etc...
- Introduce booleans as `true` or `false` and eliminate them using `if` expressions. if expressions take an expression of type `Bool` and two sub-expressions of the same type.
- Products are 2-element pairs like in Haskell. You form products like `(x, y)` and access each element using `fst` (or `π1`) and `snd` (or `π2`).
- Sums are 2-element co-pairs, formed with either `inl x:A` or `inr y:B` (assuming either `x:A` or `y:B` is in scope). `case s f g` does case analysis on `s`, passing the result to the function `f` if s was `inl` or `g` if `inr`.
- To construct terms using `case`, use _space_ to apply arguments. for instance, `case s f c` applies `s` to `case`, which then applies `f` to the `case s` and `g` to `case s f` etc...  
- Records are generalised products formed as comma-separated sequence of assignments of terms to labels `t=v`, nested inside curly braces `{u=1, idnat=\a:Nat.a}`. Record types are comma-separated assignments of typings to labels `t:v`, nested inside curly braces (such as `{1:Nat, f:Nat->Nat}`).
- Types are uppercase strings as they must be distinct from term variables.
- You can introduce a universal type with `Λ` or `L` much like you would use a function abstraction.
- You can introduce existential types with the `pack {T1,t} as T2` construct. This construct takes a concrete type `T1` like `Nat`, a term `t`, and an existential type `T2`.
- Products have the highest precedence, followed by sums, arrows, and then all other types.
- Types are either type variables, abstractions, or nested arrow types: `T -> T`. Arrows associate to the right so that `T -> T -> T` is the same as `T -> (T -> T)` but not `((T -> T) -> T)`. The product binds weaker than arrows, so `∀X.X->X` is the same as `∀X.(X->X)`. 
- Nested terms don't need brackets: `LX.LY.\x:X.\y:Y. y` unless enforcing application on the right. Whitespace does not matter `LX.\x:X.          x` unless it is between application where you need at least one space.
- To quit use `Ctrl+C` or whatever your machine uses to interrupt computations.

## Semantics

The semantics implements beta-reduction on terms and alpha-equivalence as the `Eq` instance of `SOLTerm`. The semantics are the same as SystemF with the addition of existential quantification over types, nats, bools, records, sums, and products. We reformulate the semantics as [typing judgements](https://existentialtype.wordpress.com/2011/03/27/the-holy-trinity/):

for variables (adopted from STLC):

<a href="https://www.codecogs.com/eqnedit.php?latex=\overline{\Gamma&space;\vdash&space;x:T},\quad&space;\mbox{(if&space;$x:T&space;\in&space;\Gamma$)}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\overline{\Gamma&space;\vdash&space;x:T},\quad&space;\mbox{(if&space;$x:T&space;\in&space;\Gamma$)}" title="\overline{\Gamma \vdash x:T},\quad \mbox{(if $x:T \in \Gamma$)}" /></a>

for abstractions (adopted from STLC):

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{\Gamma&space;,x:A&space;\vdash&space;t:B}{\Gamma&space;\vdash&space;(\lambda&space;x&space;:&space;A.&space;t)&space;:&space;B&space;}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{\Gamma&space;,x:A&space;\vdash&space;t:B}{\Gamma&space;\vdash&space;(\lambda&space;x&space;:&space;A.&space;t)&space;:&space;B&space;}" title="\frac{\Gamma ,x:A \vdash t:B}{\Gamma \vdash (\lambda x : A. t) : B }" /></a>

and application (adopted from STLC):

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{\Gamma&space;\vdash&space;f&space;:&space;A&space;\Rightarrow&space;B\quad&space;\Gamma&space;\vdash&space;x&space;:&space;A}{\Gamma&space;\vdash&space;(f&space;x)&space;:&space;B}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{\Gamma&space;\vdash&space;f&space;:&space;A&space;\Rightarrow&space;B\quad&space;\Gamma&space;\vdash&space;x&space;:&space;A}{\Gamma&space;\vdash&space;(f&space;x)&space;:&space;B}" title="\frac{\Gamma \vdash f : A \Rightarrow B\quad \Gamma \vdash x : A}{\Gamma \vdash (f x) : B}" /></a>

and the reduction relation (adopted from STLC):

<a href="https://www.codecogs.com/eqnedit.php?latex=(\lambda&space;x&space;:&space;T&space;.&space;M)N&space;\rightsquigarrow&space;M&space;[x&space;:=&space;N]" target="_blank"><img src="https://latex.codecogs.com/gif.latex?(\lambda&space;x&space;:&space;T&space;.&space;M)N&space;\rightsquigarrow&space;M&space;[x&space;:=&space;N]" title="(\lambda x : T . M)N \rightsquigarrow M [x := N]" /></a>

and special introduction, elimination, and reduction rules for universal types:

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{\Gamma\,,&space;X\,&space;Type&space;\vdash&space;t&space;:&space;T}{\Gamma&space;\vdash&space;(\Lambda&space;X.t):\forall&space;X.T}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{\Gamma\,,&space;X\,&space;Type&space;\vdash&space;t&space;:&space;T}{\Gamma&space;\vdash&space;(\Lambda&space;X.t):\forall&space;X.T}" title="\frac{\Gamma\,, X\, Type \vdash t : T}{\Gamma \vdash (\Lambda X.t):\forall X.T}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{\Gamma&space;\vdash&space;f&space;:&space;\forall&space;X.T}{\Gamma&space;\vdash&space;(f\,&space;A):T[X&space;:=&space;A]}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{\Gamma&space;\vdash&space;f&space;:&space;\forall&space;X.T}{\Gamma&space;\vdash&space;(f\,&space;A):T[X&space;:=&space;A]}" title="\frac{\Gamma \vdash f : \forall X.T}{\Gamma \vdash (f\, A):T[X := A]}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=(\Lambda&space;X.t)\,A&space;\rightsquigarrow&space;t[X:=A]" target="_blank"><img src="https://latex.codecogs.com/gif.latex?(\Lambda&space;X.t)\,A&space;\rightsquigarrow&space;t[X:=A]" title="(\Lambda X.t)\,A \rightsquigarrow t[X:=A]" /></a>

We have special introduction, elimination and reduction rules for products:

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{\Gamma&space;\vdash&space;a&space;:&space;A\quad&space;\Gamma&space;\vdash&space;b&space;:&space;B}{\Gamma&space;\vdash&space;(a,b):A\times&space;B}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{\Gamma&space;\vdash&space;a&space;:&space;A\quad&space;\Gamma&space;\vdash&space;b&space;:&space;B}{\Gamma&space;\vdash&space;(a,b):A\times&space;B}" title="\frac{\Gamma \vdash a : A\quad \Gamma \vdash b : B}{\Gamma \vdash (a,b):A\times B}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{\Gamma&space;\vdash&space;p:A\times&space;B}{\Gamma&space;\vdash&space;\pi_{1}\,p:A}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{\Gamma&space;\vdash&space;p:A\times&space;B}{\Gamma&space;\vdash&space;\pi_{1}\,p:A}" title="\frac{\Gamma \vdash p:A\times B}{\Gamma \vdash \pi_{1}\,p:A}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{\Gamma&space;\vdash&space;p:A\times&space;B}{\Gamma&space;\vdash&space;\pi_{2}\,p:B}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{\Gamma&space;\vdash&space;p:A\times&space;B}{\Gamma&space;\vdash&space;\pi_{2}\,p:B}" title="\frac{\Gamma \vdash p:A\times B}{\Gamma \vdash \pi_{2}\,p:B}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\pi_{1}(a,b)&space;\rightsquigarrow&space;a" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\pi_{1}(a,b)&space;\rightsquigarrow&space;a" title="\pi_{1}(a,b) \rightsquigarrow a" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\pi_{2}(a,b)&space;\rightsquigarrow&space;b" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\pi_{2}(a,b)&space;\rightsquigarrow&space;b" title="\pi_{2}(a,b) \rightsquigarrow b" /></a>

for sums:

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{\Gamma&space;\vdash&space;a:A}{\Gamma&space;\vdash&space;inl\,a&space;:&space;A&space;&plus;&space;B}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{\Gamma&space;\vdash&space;a:A}{\Gamma&space;\vdash&space;inl\,a&space;:&space;A&space;&plus;&space;B}" title="\frac{\Gamma \vdash a:A}{\Gamma \vdash inl\,a : A + B}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{\Gamma&space;\vdash&space;b:B}{\Gamma&space;\vdash&space;inr\,b&space;:&space;A&space;&plus;&space;B}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{\Gamma&space;\vdash&space;b:B}{\Gamma&space;\vdash&space;inr\,b&space;:&space;A&space;&plus;&space;B}" title="\frac{\Gamma \vdash b:B}{\Gamma \vdash inr\,b : A + B}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{\Gamma&space;\vdash&space;s:&space;A&plus;B\quad\Gamma\vdash&space;f&space;:A\rightarrow&space;X\quad\Gamma\vdash&space;g:B\rightarrow&space;X}{\Gamma&space;\vdash&space;case\,&space;s\,&space;f\,&space;g:X}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{\Gamma&space;\vdash&space;s:&space;A&plus;B\quad\Gamma\vdash&space;f&space;:A\rightarrow&space;X\quad\Gamma\vdash&space;g:B\rightarrow&space;X}{\Gamma&space;\vdash&space;case\,&space;s\,&space;f\,&space;g:X}" title="\frac{\Gamma \vdash s: A+B\quad\Gamma\vdash f :A\rightarrow X\quad\Gamma\vdash g:B\rightarrow X}{\Gamma \vdash case\, s\, f\, g:X}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=case\,(inl\,&space;a)\,f\,g\rightsquigarrow\,f\,a" target="_blank"><img src="https://latex.codecogs.com/gif.latex?case\,(inl\,&space;a)\,f\,g\rightsquigarrow\,f\,a" title="case\,(inl\, a)\,f\,g\rightsquigarrow\,f\,a" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=case\,(inr\,&space;b)\,f\,g\rightsquigarrow\,g\,b" target="_blank"><img src="https://latex.codecogs.com/gif.latex?case\,(inr\,&space;b)\,f\,g\rightsquigarrow\,g\,b" title="case\,(inr\, b)\,f\,g\rightsquigarrow\,g\,b" /></a>

Next, there is a special elimination rule for records:

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{\forall&space;i&space;.\,\Gamma&space;\vdash&space;t_i&space;:&space;T_i}{\forall&space;i.\,\Gamma&space;\vdash&space;\{l_i=t_i&space;\}:\{l_i:T_i\}}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{\forall&space;i&space;.\,\Gamma&space;\vdash&space;t_i&space;:&space;T_i}{\forall&space;i.\,\Gamma&space;\vdash&space;\{l_i=t_i&space;\}:\{l_i:T_i\}}" title="\frac{\forall i .\,\Gamma \vdash t_i : T_i}{\forall i.\,\Gamma \vdash \{l_i=t_i \}:\{l_i:T_i\}}" /></a>

with sensible structural (internal) reduction rules for terms inside records. Lastly we have rules for record projections:

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{\forall&space;i&space;.\,\Gamma&space;\vdash&space;t&space;:&space;\{l_i:T_i\}}{\forall&space;i.\,\Gamma&space;\vdash&space;t.l_i&space;:&space;T_i}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{\forall&space;i&space;.\,\Gamma&space;\vdash&space;t&space;:&space;\{l_i:T_i\}}{\forall&space;i.\,\Gamma&space;\vdash&space;t.l_i&space;:&space;T_i}" title="\frac{\forall i .\,\Gamma \vdash t : \{l_i:T_i\}}{\forall i.\,\Gamma \vdash t.l_i : T_i}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{t&space;\rightsquigarrow&space;t'}{t.l&space;\rightsquigarrow&space;t'.l}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{t&space;\rightsquigarrow&space;t'}{t.l&space;\rightsquigarrow&space;t'.l}" title="\frac{t \rightsquigarrow t'}{t.l \rightsquigarrow t'.l}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\{l_i=t_i\}.l&space;\rightsquigarrow&space;t_i" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\{l_i=t_i\}.l&space;\rightsquigarrow&space;t_i" title="\{l_i=t_i\}.l \rightsquigarrow t_i" /></a>

We have standard rules for Nats, Booleans, and if statements:

<a href="https://www.codecogs.com/eqnedit.php?latex=\overline{\Gamma&space;\vdash&space;n:Nat}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\overline{\Gamma&space;\vdash&space;n:Nat}" title="\overline{\Gamma \vdash n:Nat}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{\Gamma&space;\vdash&space;b&space;:&space;Bool\quad&space;\Gamma&space;\vdash&space;t_{1}&space;:&space;T\quad&space;\Gamma&space;\vdash&space;t_{2}&space;:&space;T}{\Gamma&space;\vdash&space;\tt{if}\,b\,\tt{then}\,t_{1}\,\tt{else}\,t_{2}&space;:&space;T}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{\Gamma&space;\vdash&space;b&space;:&space;Bool\quad&space;\Gamma&space;\vdash&space;t_{1}&space;:&space;T\quad&space;\Gamma&space;\vdash&space;t_{2}&space;:&space;T}{\Gamma&space;\vdash&space;\tt{if}\,b\,\tt{then}\,t_{1}\,\tt{else}\,t_{2}&space;:&space;T}" title="\frac{\Gamma \vdash b : Bool\quad \Gamma \vdash t_{1} : T\quad \Gamma \vdash t_{2} : T}{\Gamma \vdash \tt{if}\,b\,\tt{then}\,t_{1}\,\tt{else}\,t_{2} : T}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex={\tt&space;if}\,{\tt&space;false\,&space;then}\,M\,{\tt&space;else}N&space;\rightsquigarrow&space;N&space;\newline&space;{\tt&space;if}\,{\tt&space;true\,&space;then}\,M\,{\tt&space;else}N&space;\rightsquigarrow&space;M" target="_blank"><img src="https://latex.codecogs.com/gif.latex?{\tt&space;if}\,{\tt&space;false\,&space;then}\,M\,{\tt&space;else}N&space;\rightsquigarrow&space;N&space;\newline&space;{\tt&space;if}\,{\tt&space;true\,&space;then}\,M\,{\tt&space;else}N&space;\rightsquigarrow&space;M" title="{\tt if}\,{\tt false\, then}\,M\,{\tt else}N \rightsquigarrow N \newline {\tt if}\,{\tt true\, then}\,M\,{\tt else}N \rightsquigarrow M" /></a>

Finally, we have special introduction and elimination rules for existentials:

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{\Gamma&space;\vdash&space;t:T[Y&space;:=&space;X]}{\Gamma&space;\vdash&space;{\tt&space;pack}\,\{X,&space;t\}\,{\tt&space;as}\,\{\exists&space;Y&space;.&space;T\}:\{\exists&space;Y.T\}}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{\Gamma&space;\vdash&space;t:T[Y&space;:=&space;X]}{\Gamma&space;\vdash&space;{\tt&space;pack}\,\{X,&space;t\}\,{\tt&space;as}\,\{\exists&space;Y&space;.&space;T\}:\{\exists&space;Y.T\}}" title="\frac{\Gamma \vdash t:T[Y := X]}{\Gamma \vdash {\tt pack}\,\{X, t\}\,{\tt as}\,\{\exists Y . T\}:\{\exists Y.T\}}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{\Gamma&space;\vdash&space;t_1:\exists&space;X.T_1\quad\Gamma,&space;X\,&space;Type,&space;x:&space;T_1&space;\vdash&space;t_2&space;:&space;T_2}{\Gamma&space;\vdash&space;{\tt&space;unpack}\,\{X,&space;x\}&space;=&space;t_1\,{\tt&space;in}\,t_2:T_2}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{\Gamma&space;\vdash&space;t_1:\exists&space;X.T_1\quad\Gamma,&space;X\,&space;Type,&space;x:&space;T_1&space;\vdash&space;t_2&space;:&space;T_2}{\Gamma&space;\vdash&space;{\tt&space;unpack}\,\{X,&space;x\}&space;=&space;t_1\,{\tt&space;in}\,t_2:T_2}" title="\frac{\Gamma \vdash t_1:\exists X.T_1\quad\Gamma, X\, Type, x: T_1 \vdash t_2 : T_2}{\Gamma \vdash {\tt unpack}\,\{X, x\} = t_1\,{\tt in}\,t_2:T_2}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex={\tt&space;unpack}\,&space;\{Y,y\}&space;=&space;({\tt&space;pack}&space;\{X,x\}\,{\tt&space;as}\,T_1)&space;{\tt&space;in}\,&space;t_2&space;\rightsquigarrow&space;t_2[y&space;:=&space;x][Y&space;:=&space;X]" target="_blank"><img src="https://latex.codecogs.com/gif.latex?{\tt&space;unpack}\,&space;\{Y,y\}&space;=&space;({\tt&space;pack}&space;\{X,x\}\,{\tt&space;as}\,T_1)&space;{\tt&space;in}\,&space;t_2&space;\rightsquigarrow&space;t_2[y&space;:=&space;x][Y&space;:=&space;X]" title="{\tt unpack}\, \{Y,y\} = ({\tt pack} \{X,x\}\,{\tt as}\,T_1) {\tt in}\, t_2 \rightsquigarrow t_2[y := x][Y := X]" /></a>

As well as sensible reduction rules under pack/unpack statements. 

- This means the typing context now also contains types, and types occur in terms. The phrase `X Type` means X is a type. We do not implement Agda style type hierarchies here.
- Variables, abstraction, sums, products, booleans, and Nats follow semantics that are fairly standard in the literature.
- Records are typeable only if all of their subterms are typeable and all of the labels are unique. We leverage the STLC typing rules and the subtyping relation to ensure record fields are typeable.
- Projections are typeable only if it is applied to a term which is a well-typed record and the projection label (`x` in `{x=2}.x`) explicitly matches a label in that record.
- The pack statement enables you to package up a witness type `T1` with a term `t`. The witness type here is known as the hidden representation type which must be provided as the type checker cannot infer the type of an existential in general. For instance `pack {Nat, {z=\x:Nat.x}} as EX.{z:X->X}` has existential type `∃X.{z:X->X}` but also `∃X.{z:Nat->X}`. We can then pass this into a function as a 'module' of sorts, where any term with a witness type to a given interface can be used.
- We can use existential types by `unpack`ing them. This construct takes an existential type, binding them to the type variable `X` and a term variable `x`. We may then use `x` in the body of the expression. However, the representation type is hidden during typechecking so that we may only use operations allowed by the abstract type in the body. This enforces that uses adhere to an interface so that concrete implementations may be swapped out as needed without breaking abstraction boundaries.
- This implementation follows a [small-step](https://cs.stackexchange.com/questions/43294/difference-between-small-and-big-step-operational-semantics) operational semantics and Berendregt's [variable convention](https://cs.stackexchange.com/questions/69323/barendregts-variable-convention-what-does-it-mean) (see `substitutite` in SOL.hs). The variable convention is adopted for both types and terms.
- Reductions include the one-step reduction (see `reduce1` in SOL.hs), the many-step reduction (see `reduce` in SOL.hs). 

## Other Implementation Details
- SOL.hs contains the Haskell implementation of the calculus, including substitution, reduction, and other useful things.
- Parser.hs contains the monadic parser combinators needed to parse input strings into typed-term ASTs for the calculus.
- Repl.hs contains a simple read-eval-print loop which hooks into main, and into the parser.
- Main.hs is needed for GHC to compile without any flags, it also invokes the repl.

For contributions, see the project to-do list or submit a PR with something you think it needs.




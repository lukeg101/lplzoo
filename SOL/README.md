# SOL
Haskell implementation on John Mitchell and Gordon Plotkin's [SOL](http://theory.stanford.edu/~jcm/papers/mitch-plotkin-88.pdf). It has the features of System F but with existential types made explicit.

We have added `Nat`ural numbers, `Bool`eans, records, product, and sum (disjoint union) types to make this calculus slightly more interesting as Mitchell and Plotkin do. These types are not necessary for SOL, and [others](https://www.cs.cornell.edu/courses/cs4110/2018fa/lectures/lecture26.pdf) have presented existentials with just STLC, however we use this presentation to make clear their relation with parametricity.

This strongly normalising calculus makes it easier to see how abstract data types, objects, and (simplistic) modules can arise as a special case of parametric polymorphism.

## Prerequisites
You need [Haskell](https://www.haskell.org/), this compiles with GHC 8.2.2 at least (Stack resolver: lts-11.0).

## To Build & Run

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
- `(\x:Bool.x) true` booleans are supportedf
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
∃X,{init:X, set:X->X}
>   tLX.\x:X. x
∀X.X->X
>   tLX.\x:X. x x
Cannot Type Term: LX.\x:X. x x
```
where `∀Y.(Y->Y)->Y->Y` is equivalent to the System F/SOL type for `Nats`.

Note: if you provide a non-normalizing term, the type checker will fail and reduction will not occur.

You can save terms for the life of the program with a `let` expression. Any time a saved variable appears in a term, it will be substituted for the saved term:
```
>   let foo = pack {Nat, {init=0, set=\x:Nat.x}} as EX.{init:X, set:X->X}
Saved term: pack {Nat, {init=0, set=λx:Nat.x}} as ∃X,{init:X, set:X->X}
>   unpack {X,body} = foo in body
~>* {init=0, set=λx:Nat.x}
>   unpack {X,body} = foo in body.init
~>* 0
```

Note: Consequently `let` and `=` are keywords, and so you cannot name variables with these. Additionally `L`, `[`, `]`, and `P`, `E`, `pack`, `unpack`, `true`, `false`, `fst`, `snd`, `inl`, `inr`, `case`, `*`, `+`, `Nat`, `Bool`, `{`, `}`, `as`, `(`, `)`, and `in` are keywords in SOL.

Additionally we have type level `lett` statements that allow you to define and use types:
```
>   lett REGISTER = EX.{init:X, set:X->X}
Saved type: ∃X,{init:X, set:X->X}
>   let reg = pack {Nat, {init=0, set=λx:Nat.x}} as REGISTER
Saved term: pack {Nat, {init=0, set=λx:Nat.x}} as ∃X,{init:X, set:X->X}
>   unpack {R,register} = reg in register.init
~>* 0
```
This makes it easier to define both terms and types, but does not allow type level application (See Omega). `lett` is also a keyword.

## Syntax 

We base the language on the BNF for the typed calculus:

<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}&space;\mathbf{\tau}&&space;::=&space;&&space;\lambda&space;\mathbf{\upsilon}{\tt&space;:}\sigma&space;.&space;\mathbf{\tau}\\&space;&&space;|&space;&&space;\tau\,&space;\tau\\&space;&&space;|&space;&&space;\upsilon&space;\\&space;&&space;|&space;&&space;\Lambda&space;\mu.\tau\\&space;&|&&space;[\sigma]&space;&&\\&|&&space;inl\,\tau:\sigma\\&space;&|&&space;inr\,\tau:\sigma\\&space;&|&&space;case\,\tau\,\tau\,\tau\\&space;&|&&space;(\tau,&space;\tau)\\&space;&|&&space;\pi_{1}/fst\,\tau\\&space;&|&&space;\pi_{2}/snd\,\tau\\\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}&space;\mathbf{\tau}&&space;::=&space;&&space;\lambda&space;\mathbf{\upsilon}{\tt&space;:}\sigma&space;.&space;\mathbf{\tau}\\&space;&&space;|&space;&&space;\tau\,&space;\tau\\&space;&&space;|&space;&&space;\upsilon&space;\\&space;&&space;|&space;&&space;\Lambda&space;\mu.\tau\\&space;&|&&space;[\sigma]&space;&&\\&|&&space;inl\,\tau:\sigma\\&space;&|&&space;inr\,\tau:\sigma\\&space;&|&&space;case\,\tau\,\tau\,\tau\\&space;&|&&space;(\tau,&space;\tau)\\&space;&|&&space;\pi_{1}/fst\,\tau\\&space;&|&&space;\pi_{2}/snd\,\tau\\\end{matrix}" title="\begin{matrix} \mathbf{\tau}& ::= & \lambda \mathbf{\upsilon}{\tt :}\sigma . \mathbf{\tau}\\ & | & \tau\, \tau\\ & | & \upsilon \\ & | & \Lambda \mu.\tau\\ &|& [\sigma] &&\\&|& inl\,\tau:\sigma\\ &|& inr\,\tau:\sigma\\ &|& case\,\tau\,\tau\,\tau\\ &|& (\tau, \tau)\\ &|& \pi_{1}/fst\,\tau\\ &|& \pi_{2}/snd\,\tau\end{matrix}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}&space;&|&&space;\tt{true}&space;&&\\&space;&|&&space;\tt{false}&space;&&\\&space;&|&&space;\eta&space;&&\\&space;&|&&space;\{\upsilon=&space;\tau,...\}&space;\\&space;&|&\tau&space;.&space;\upsilon\\&space;&|&&space;pack\,\{\sigma,\tau&space;\}\,&space;as\,&space;\sigma&space;\\&space;&|&&space;unpack\,\{\mu,\upsilon&space;\}\,=\tau\,&space;as\,&space;\tau&space;\\&space;\\\eta&space;&&space;::=&space;&&space;\tt{0}&space;|&space;\tt{1}&space;|&space;\tt{3}&space;|&space;...&space;|&space;\tt{42}&space;|&space;...&space;\\\upsilon&space;&&space;::=&space;&&space;\tt{a}&space;|&space;\tt{b}&space;|&space;\tt{c}&space;|&space;...&space;|&space;\tt{aa}&space;|&space;...&space;\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}&space;&|&&space;\tt{true}&space;&&\\&space;&|&&space;\tt{false}&space;&&\\&space;&|&&space;\eta&space;&&\\&space;&|&&space;\{\upsilon=&space;\tau,...\}&space;\\&space;&|&\tau&space;.&space;\upsilon\\&space;&|&&space;pack\,\{\sigma,\tau&space;\}\,&space;as\,&space;\sigma&space;\\&space;&|&&space;unpack\,\{\mu,\upsilon&space;\}\,=\tau\,&space;as\,&space;\tau&space;\\&space;\\\eta&space;&&space;::=&space;&&space;\tt{0}&space;|&space;\tt{1}&space;|&space;\tt{3}&space;|&space;...&space;|&space;\tt{42}&space;|&space;...&space;\\\upsilon&space;&&space;::=&space;&&space;\tt{a}&space;|&space;\tt{b}&space;|&space;\tt{c}&space;|&space;...&space;|&space;\tt{aa}&space;|&space;...&space;\end{matrix}" title="\begin{matrix} &|& \tt{true} &&\\ &|& \tt{false} &&\\ &|& \eta &&\\ &|& \{\upsilon= \tau,...\} \\ &|&\tau . \upsilon\\ &|& pack\,\{\sigma,\tau \}\, as\, \sigma \\ &|& unpack\,\{\mu,\upsilon \}\,=\tau\, as\, \tau \\ \\\eta & ::= & \tt{0} | \tt{1} | \tt{3} | ... | \tt{42} | ... \\\upsilon & ::= & \tt{a} | \tt{b} | \tt{c} | ... | \tt{aa} | ... \end{matrix}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}&space;\sigma&space;&&space;::=&space;&&space;{\tt&space;\mu&space;}\\&space;&&space;|&space;&&space;\sigma&space;\rightarrow&space;\sigma\\&space;&|&&space;\forall&space;\mu&space;.&space;\sigma\\&space;&|&\exists&space;\mu&space;.&space;\sigma\\&space;&|&&space;Nat\\&space;&|&Bool\\&|&\{\mathbf{\upsilon}\tt{:}\sigma,...\}\\\\&space;\mu&space;&&space;::=&space;&&space;\tt{A}&space;|&space;\tt{B}&space;|&space;\tt{C}&space;|&space;...&space;|&space;\tt{AA}&space;|&space;...&space;\\&space;\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}&space;\sigma&space;&&space;::=&space;&&space;{\tt&space;\mu&space;}\\&space;&&space;|&space;&&space;\sigma&space;\rightarrow&space;\sigma\\&space;&|&&space;\forall&space;\mu&space;.&space;\sigma\\&space;&|&\exists&space;\mu&space;.&space;\sigma\\&space;&|&&space;Nat\\&space;&|&Bool\\&|&\{\mathbf{\upsilon}\tt{:}\sigma,...\}\\\\&space;\mu&space;&&space;::=&space;&&space;\tt{A}&space;|&space;\tt{B}&space;|&space;\tt{C}&space;|&space;...&space;|&space;\tt{AA}&space;|&space;...&space;\\&space;\end{matrix}" title="\begin{matrix} \sigma & ::= & {\tt \mu }\\ & | & \sigma \rightarrow \sigma\\ &|& \forall \mu . \sigma\\ &|&\exists \mu . \sigma\\ &|& Nat\\ &|&Bool\\&|&\{\mathbf{\upsilon}\tt{:}\sigma,...\}\\\\ \mu & ::= & \tt{A} | \tt{B} | \tt{C} | ... | \tt{AA} | ... \\ \end{matrix}" /></a>

However we adopt standard bracketing conventions to eliminate ambiguity in the parser. Concretely, the parser implements the non-ambiguous grammar for terms as follows:

TODO

and types:

TODO

with term variables:

TODO 

and type variables:

TODO

Some notes about the syntax:

- The above syntax only covers the core calculus, and not the repl extensions (such as let bindings above). The extensions are simply added on in the repl.
- Variables are strings (excluding numbers), as this is isomorphic to a whiteboard treatment and hence the most familiar.
- Types are uppercase strings for the same reasons. Type variables must be distinct from term variables.
- Types are either type variables, abstractions, or nested arrow types: `T -> T`. Arrows associate to the right so that `T -> T -> T` is the same as `T -> (T -> T)` but not `((T -> T) -> T)`. The product binds weaker than arrows, so `∀X.X->X` is the same as `∀X.(X->X)`. 
- Nested terms don't need brackets: `LX.LY.\x:X.\y:Y. y` unless enforcing application on the right. Whitespace does not matter `LX.\x:X.          x` unless it is between application where you need at least one space.
- To quit use `Ctrl+C` or whatever your machine uses to interrupt computations.

## Semantics

The semantics implements beta-reduction on terms and alpha-equivalence as the `Eq` instance of `SOLTerm`. The semantics are the same as SystemF with the addition of existential quantification over types, nats, bools, records, sums, and products. We reformulate the semantics as [typing judgements](https://existentialtype.wordpress.com/2011/03/27/the-holy-trinity/):

for variables (adopted from STLC):

TODO

for abstractions (adopted from STLC):

TODO

and application (adopted from STLC):

TODO

and the reduction relation (adopted from STLC):

TODO

and special introduction, elimination, and reduction rules for types:

TODO


- This means the typing context now also contains types, and types occur in terms. The phrase `X Type` means X is a type. We do not implement Agda style type hierarchies here.
- This implementation follows a [small-step](https://cs.stackexchange.com/questions/43294/difference-between-small-and-big-step-operational-semantics) operational semantics and Berendregt's [variable convention](https://cs.stackexchange.com/questions/69323/barendregts-variable-convention-what-does-it-mean) (see `substitutite` in SOL.hs). The variable convention is adopted for both types and terms.
- Reductions include the one-step reduction (see `reduce1` in SOL.hs), the many-step reduction (see `reduce` in SOL.hs). 

## Other Implementation Details
- SOL.hs contains the Haskell implementation of the calculus, including substitution, reduction, and other useful things.
- Parser.hs contains the monadic parser combinators needed to parse input strings into typed-term ASTs for the calculus.
- Repl.hs contains a simple read-eval-print loop which hooks into main, and into the parser.
- Main.hs is needed for GHC to compile without any flags, it also invokes the repl.

For contributions, see the project to-do list or submit a PR with something you think it needs.

Work initially documented [here](https://gist.github.com/lukeg101/f1c13024cf9ccbeaff3c3553baca037f).



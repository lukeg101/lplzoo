# Edinburgh Logical Framework (first-order dependent types)
Haskell implementation of Bob Harper, Furio Honsell, and Gordon Plotkin's [Edinburgh Logical Framework](https://dl.acm.org/citation.cfm?id=138060) which captures first-order dependent types. It is STLC, but with a generalised Pi type, type-level application of terms, and type families. Dependent type machinery isn't useful on its own so we add Nats, and size-dependent Vectors of Nats to demonstrate how to instantiate types based on the size of their terms. We do not want to conflate LF with [System F](https://github.com/lukeg101/lplzoo/tree/master/SystemF), and so do not include term-level types or polymorphism.

This strongly normalising calculus works on the notion of terms that depend on types under the banner of [propositions as types](https://www.youtube.com/watch?v=SknxggwRPzU). Dependent types allow us to encode predicate logic in a language with programs as witness or proof of propositions. 

## Prerequisites
You need [Haskell](https://www.haskell.org/), this compiles with GHC 8.2.2 at least (Stack resolver: lts-11.0).

## To Build & Run

To compile and run do:
`ghc -O2 -o lf Main`
then run `./lf`

Alternatively to use the GHCi Interpreter do:
`ghci Main`
then type `main`

In either case you get something like the following:
```
Welcome to the LF REPL
Type some terms or press Enter to leave.
>
```

Note: When run in GHCi, you don't have the luxury of escaped characters, backspace, delete etc...
Compile it using GHC if you need this.

## Examples 
Where you can then have some fun, try these examples:
- `\x:Nat.x` the identity function on Nats.
- `nil` the empty list of type `Vec Nat 0`
- `cons 0 1 nil` the singleton list of type `Vec Nat 1`
- `cons 1 42 (cons 0 43 nil)` a two element list.
- `(\n:Nat.cons n) 2 42` a function that takes a number and instantiates `cons 2` with type `Nat->Vec Nat 2->Vec Nat (succ 2)`. 42 is then applied to return a term of type `Vec Nat (succ 2)` 
- `(\n:Nat. cons (succ n)) 0 42 (cons 0 9001 nil)` Similar to the above but a two element set with 42 and 9001 in it

Note: `Π` is the dependent product type and `Πx:A.B` is the same as `A -> B` when `x` does not occur in `B`. `Π` is alternatively typed as `P`, respectively.

The parser is smart enough to recognise λ, Π , Nat, and Vec; so you can copy and paste from the output:
```
>   \x:Nat.x
=   λx:Nat.x
>   (\x:Vec Nat 1.x) (cons 0 42 nil)
~>* cons 0 42 nil
```

`>` denotes the REPL waiting for input, `=` means no reductions occurred (it's the same term), `~>` denotes one reduction, and `~>*` denotes 0 or more reductions (although in practice this is 1 or more due to `=`).

There is also a reduction tracer, which should print each reduction step. prefix any string with `'` in order to see the reductions:
```
>   '(\n:Nat.\x:Nat.cons (succ n) x) 0 42 (cons 0 42 nil)
~>  (λx:Nat.cons (succ 0) x) 42 (cons 0 42 nil)
~>  cons (succ 0) 42 (cons 0 42 nil)
~>  cons 1 42 (cons 0 42 nil)
```
Note: `succ n` stands for the `succ`essor of `n`, [Peano](https://en.wikipedia.org/wiki/Peano_axioms) style.

There is also a typing mechanism, which should display the type or fail as usual.
```
>   (\x:Vec Nat 4.x) nil
Cannot Type Term: (λx:Vec Nat 4.x) nil
>   (\x:Vec Nat 0.x) nil
~>* nil
```

Note: if you provide a non-normalizing term, the type checker will fail and reduction will not occur.

You can save terms for the life of the program with a `let` expression. Any time a saved variable appears in a term, it will be substituted for the saved term:
```
>   let singleton = \x:Nat.cons 0 x nil
Saved term: λx:Nat.cons 0 x nil
```

Note: Consequently `let` and `=` are keywords, and so you cannot name variables with these. Additionally `Nat`, `succ`, `cons`, `nil`, and `Pi` are keywords in LF.

Note: We include introduction rules for Nats to show type-level term substitution works. We do not include Nat [eliminators](https://www.andres-loeh.de/LambdaPi/LambdaPi.pdf) or any notion of type-based [polymorphism](https://github.com/lukeg101/lplzoo/tree/master/FOmega) however as the focus is on dependent types. Use the calculus with this in mind, like the next example.

Additionally we have type level `lett` statements that allow you to define and use types:
```
>   lett VecZ = Vec Nat 0
Saved type: Vec Nat 0
>   (\x:VecZ.x) nil
~>* nil
```
Submit a PR with eliminators for Vectors and Natural numbers, or perhaps some machinery to introduce term constructors of a given type.

This makes it easier to define both terms and types, but does not allow type-level application (See Omega) or type-based polymorphism (see SystemF). `lett` is also a keyword.

## Syntax 

We base the language on the BNF for the typed calculus:

<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}&space;\mathbf{\tau}&&space;::=&space;&&space;\lambda&space;\mathbf{\upsilon}{\tt&space;:}\sigma&space;.&space;\mathbf{\tau}\\&space;&&space;|&space;&&space;\tau\,&space;\tau\\&space;&&space;|&space;&&space;\upsilon&space;\\&space;&|&&space;{\tt&space;nil}&space;\\&space;&|&&space;{\tt&space;cons}\,\tau\,\tau\,\tau&space;\\&|&{\tt&space;succ}\,&space;\tau\\&|&\eta&space;\\&&\\&space;\upsilon&space;&&space;::=&space;&&space;\tt{a}&space;|&space;\tt{b}&space;|&space;\tt{c}&space;|&space;...&space;|&space;\tt{aa}&space;|&space;...\\&space;\eta&space;&&space;::=&space;&&space;\tt{0}&space;|&space;\tt{1}&space;|&space;\tt{2}&space;|&space;...&space;|&space;\tt{42}&space;|&space;...&space;\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}&space;\mathbf{\tau}&&space;::=&space;&&space;\lambda&space;\mathbf{\upsilon}{\tt&space;:}\sigma&space;.&space;\mathbf{\tau}\\&space;&&space;|&space;&&space;\tau\,&space;\tau\\&space;&&space;|&space;&&space;\upsilon&space;\\&space;&|&&space;{\tt&space;nil}&space;\\&space;&|&&space;{\tt&space;cons}\,\tau\,\tau\,\tau&space;\\&|&{\tt&space;succ}\,&space;\tau\\&|&\eta&space;\\&&\\&space;\upsilon&space;&&space;::=&space;&&space;\tt{a}&space;|&space;\tt{b}&space;|&space;\tt{c}&space;|&space;...&space;|&space;\tt{aa}&space;|&space;...\\&space;\eta&space;&&space;::=&space;&&space;\tt{0}&space;|&space;\tt{1}&space;|&space;\tt{2}&space;|&space;...&space;|&space;\tt{42}&space;|&space;...&space;\end{matrix}" title="\begin{matrix} \mathbf{\tau}& ::= & \lambda \mathbf{\upsilon}{\tt :}\sigma . \mathbf{\tau}\\ & | & \tau\, \tau\\ & | & \upsilon \\ &|& {\tt nil} \\ &|& {\tt cons}\,\tau\,\tau\,\tau \\&|&{\tt succ}\, \tau\\&|&\eta \\&&\\ \upsilon & ::= & \tt{a} | \tt{b} | \tt{c} | ... | \tt{aa} | ...\\ \eta & ::= & \tt{0} | \tt{1} | \tt{2} | ... | \tt{42} | ... \end{matrix}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}&space;\sigma&space;&&space;::=&space;&&space;\sigma\,{\tt&space;space}\,\tau\\&space;&&space;|&space;&&space;\sigma&space;\rightarrow&space;\sigma&space;\\&space;&|&&space;Nat\\&space;&|&&space;Vec\,\sigma\,\tau\\&space;&|&&space;\Pi&space;\theta::\kappa&space;.&space;\sigma\\&space;\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}&space;\sigma&space;&&space;::=&space;&&space;\sigma\,{\tt&space;space}\,\tau\\&space;&&space;|&space;&&space;\sigma&space;\rightarrow&space;\sigma&space;\\&space;&|&&space;Nat\\&space;&|&&space;Vec\,\sigma\,\tau\\&space;&|&&space;\Pi&space;\theta::\kappa&space;.&space;\sigma\\&space;\end{matrix}" title="\begin{matrix} \sigma & ::= & \sigma\,{\tt space}\,\tau\\ & | & \sigma \rightarrow \sigma \\ &|& Nat\\ &|& Vec\,\sigma\,\tau\\ &|& \Pi \theta::\kappa . \sigma\\ \end{matrix}" /></a>

However we adopt standard bracketing conventions to eliminate ambiguity in the parser. Concretely, the parser implements the non-ambiguous grammar for terms as follows:

TODO

and types:

TODO


Some notes about the syntax:

- The above syntax only covers the core calculus, and not the repl extensions (such as let bindings above). The extensions are simply added on in the repl.
- Term and type variables are strings (excluding numbers), as this is isomorphic to a whiteboard treatment and hence the most familiar. Term variables are lower case whereas type variables are upper case.
- TODO
- To quit use `Ctrl+C` or whatever your machine uses to interrupt computations.

## Semantics

The semantics implements beta-reduction on terms and typed definitional equality as the `Eq` instance of `T`. The term semantics are the same as STLC with the addition of dependent abstraction of types over terms, term-dependent vectors and natural numbers. We reformulate the semantics as [typing judgements](https://existentialtype.wordpress.com/2011/03/27/the-holy-trinity/):

TODO 

- TODO
- This implementation follows a [small-step](https://cs.stackexchange.com/questions/43294/difference-between-small-and-big-step-operational-semantics) operational semantics and Berendregt's [variable convention](https://cs.stackexchange.com/questions/69323/barendregts-variable-convention-what-does-it-mean) (see `substitution` in LF.hs).
- Reductions include the one-step reduction (see `reduce1` in LF.hs), the many-step reduction (see `reduce` in LF.hs). Additionally there is a one-step type-level reduction (see `reduce1T` in LF.hs).

## Other Implementation Details
- LF.hs contains the Haskell implementation of the calculus, including substitution, reduction, and other useful things.
- Parser.hs contains the monadic parser combinators needed to parse input strings into typed-term ASTs for the calculus.
- Repl.hs contains a simple read-eval-print loop which hooks into main, and into the parser.
- Main.hs is needed for GHC to compile without any flags, it also invokes the repl.

For contributions, see the project to-do list or submit a PR with something you think it needs.
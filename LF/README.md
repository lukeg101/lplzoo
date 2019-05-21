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
- `(\n:Nat. cons (succ n)) 0 42 (cons 0 9001 nil)` Similar to the above but a two element set with 42 and 9001 in it.
- `\x:(Pi n:Nat.Vec Nat n) 0.x`

Note: `Π` is the dependent product type and `Πx:A.B` is the same as `A -> B` when `x` does not occur in `B`. `Π` is alternatively typed as `Pi`, respectively.

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
>    let singleton = \x:Nat.cons 0 x nil
Saved term: λx:Nat.cons 0 x nil
>   singleton 2
~>* cons 0 2 nil
```

Note: Consequently `let` and `=` are keywords, and so you cannot name variables with these. Additionally `Nat`, `succ`, `cons`, `nil`, and `Pi` are keywords in LF.

Note: We include introduction rules for Nats to show type-level term substitution works. We do not include Nat [eliminators](https://www.andres-loeh.de/LambdaPi/LambdaPi.pdf) or any notion of type-based [polymorphism](https://github.com/lukeg101/lplzoo/tree/master/FOmega) however as the focus is on dependent types. Use the calculus with this in mind, like the next example.

Additionally we have type level `lett` statements that allow you to define and use types:
```
>   lett VECZ = Vec Nat 0
Saved type: Vec Nat 0
>   (\x:VECZ.x) nil
~>* nil
```
Submit a PR with eliminators for Vectors and Natural numbers, or perhaps some machinery to introduce term constructors of a given type.

This makes it easier to define both terms and types, but does not allow type-level application (See Omega) or type-based polymorphism (see SystemF). `lett` is also a keyword.

## Syntax 

We base the language on the BNF for the typed calculus:

<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}&space;\mathbf{\tau}&&space;::=&space;&&space;\lambda&space;\mathbf{\upsilon}{\tt&space;:}\sigma&space;.&space;\mathbf{\tau}\\&space;&&space;|&space;&&space;\tau\,&space;\tau\\&space;&&space;|&space;&&space;\upsilon&space;\\&space;&|&&space;{\tt&space;nil}&space;\\&space;&|&&space;{\tt&space;cons}\,\tau\,\tau\,\tau&space;\\&|&{\tt&space;succ}\,&space;\tau\\&|&\eta&space;\\&&\\&space;\upsilon&space;&&space;::=&space;&&space;\tt{a}&space;|&space;\tt{b}&space;|&space;\tt{c}&space;|&space;...&space;|&space;\tt{aa}&space;|&space;...\\&space;\eta&space;&&space;::=&space;&&space;\tt{0}&space;|&space;\tt{1}&space;|&space;\tt{2}&space;|&space;...&space;|&space;\tt{42}&space;|&space;...&space;\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}&space;\mathbf{\tau}&&space;::=&space;&&space;\lambda&space;\mathbf{\upsilon}{\tt&space;:}\sigma&space;.&space;\mathbf{\tau}\\&space;&&space;|&space;&&space;\tau\,&space;\tau\\&space;&&space;|&space;&&space;\upsilon&space;\\&space;&|&&space;{\tt&space;nil}&space;\\&space;&|&&space;{\tt&space;cons}\,\tau\,\tau\,\tau&space;\\&|&{\tt&space;succ}\,&space;\tau\\&|&\eta&space;\\&&\\&space;\upsilon&space;&&space;::=&space;&&space;\tt{a}&space;|&space;\tt{b}&space;|&space;\tt{c}&space;|&space;...&space;|&space;\tt{aa}&space;|&space;...\\&space;\eta&space;&&space;::=&space;&&space;\tt{0}&space;|&space;\tt{1}&space;|&space;\tt{2}&space;|&space;...&space;|&space;\tt{42}&space;|&space;...&space;\end{matrix}" title="\begin{matrix} \mathbf{\tau}& ::= & \lambda \mathbf{\upsilon}{\tt :}\sigma . \mathbf{\tau}\\ & | & \tau\, \tau\\ & | & \upsilon \\ &|& {\tt nil} \\ &|& {\tt cons}\,\tau\,\tau\,\tau \\&|&{\tt succ}\, \tau\\&|&\eta \\&&\\ \upsilon & ::= & \tt{a} | \tt{b} | \tt{c} | ... | \tt{aa} | ...\\ \eta & ::= & \tt{0} | \tt{1} | \tt{2} | ... | \tt{42} | ... \end{matrix}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}&space;\sigma&space;&&space;::=&space;&&space;\sigma\,{\tt&space;space}\,\tau\\&space;&&space;|&space;&&space;\sigma&space;\rightarrow&space;\sigma&space;\\&space;&|&&space;Nat\\&space;&|&&space;Vec\,\sigma\,\tau\\&space;&|&&space;\Pi&space;\upsilon:\sigma.&space;\sigma\\&space;\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}&space;\sigma&space;&&space;::=&space;&&space;\sigma\,{\tt&space;space}\,\tau\\&space;&&space;|&space;&&space;\sigma&space;\rightarrow&space;\sigma&space;\\&space;&|&&space;Nat\\&space;&|&&space;Vec\,\sigma\,\tau\\&space;&|&&space;\Pi&space;\upsilon:\sigma.&space;\sigma\\&space;\end{matrix}" title="\begin{matrix} \sigma & ::= & \sigma\,{\tt space}\,\tau\\ & | & \sigma \rightarrow \sigma \\ &|& Nat\\ &|& Vec\,\sigma\,\tau\\ &|& \Pi \upsilon:\sigma. \sigma\\ \end{matrix}" /></a>

However we adopt standard bracketing conventions to eliminate ambiguity in the parser. Concretely, the parser implements the non-ambiguous grammar for terms as follows:

<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}&space;&&\\&space;\mathbf{\tau}&&space;::=&space;&&space;\lambda&space;\mathbf{\upsilon}\tt{:}\sigma&space;.&space;\mathbf{\tau}\\&space;&&space;|&space;&&space;\alpha\\&space;&&\\&space;\alpha&space;&&space;::=&space;&&space;\beta&space;\\&space;&|&space;&\mathbf{\alpha\,&space;\tt{space}\,&space;\beta}&space;\\&space;&&\\&space;\beta&space;&&space;::=&space;&&space;\tt{(}\tau&space;\tt{)}\\&space;&|&&space;\upsilon&space;\\&space;&|&&space;{\tt&space;nil}\\&space;&|&&space;{\tt&space;cons}\\&space;&|&&space;{\tt&space;succ}\\&space;&|&\eta&space;\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}&space;&&\\&space;\mathbf{\tau}&&space;::=&space;&&space;\lambda&space;\mathbf{\upsilon}\tt{:}\sigma&space;.&space;\mathbf{\tau}\\&space;&&space;|&space;&&space;\alpha\\&space;&&\\&space;\alpha&space;&&space;::=&space;&&space;\beta&space;\\&space;&|&space;&\mathbf{\alpha\,&space;\tt{space}\,&space;\beta}&space;\\&space;&&\\&space;\beta&space;&&space;::=&space;&&space;\tt{(}\tau&space;\tt{)}\\&space;&|&&space;\upsilon&space;\\&space;&|&&space;{\tt&space;nil}\\&space;&|&&space;{\tt&space;cons}\\&space;&|&&space;{\tt&space;succ}\\&space;&|&\eta&space;\end{matrix}" title="\begin{matrix} &&\\ \mathbf{\tau}& ::= & \lambda \mathbf{\upsilon}\tt{:}\sigma . \mathbf{\tau}\\ & | & \alpha\\ &&\\ \alpha & ::= & \beta \\ &| &\mathbf{\alpha\, \tt{space}\, \beta} \\ &&\\ \beta & ::= & \tt{(}\tau \tt{)}\\ &|& \upsilon \\ &|& {\tt nil}\\ &|& {\tt cons}\\ &|& {\tt succ}\\ &|&\eta \end{matrix}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}\upsilon&space;&&space;::=&space;&&space;\tt{a}&space;|&space;\tt{b}&space;|&space;\tt{c}&space;|&space;...&space;|&space;\tt{aa}&space;|&space;...&space;&&\\&space;\eta&space;&&space;::=&space;&&space;\tt{0}&space;|&space;\tt{1}&space;|&space;\tt{2}&space;|&space;...&space;|&space;\tt{42}&space;|&space;...\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}\upsilon&space;&&space;::=&space;&&space;\tt{a}&space;|&space;\tt{b}&space;|&space;\tt{c}&space;|&space;...&space;|&space;\tt{aa}&space;|&space;...&space;&&\\&space;\eta&space;&&space;::=&space;&&space;\tt{0}&space;|&space;\tt{1}&space;|&space;\tt{2}&space;|&space;...&space;|&space;\tt{42}&space;|&space;...\end{matrix}" title="\begin{matrix}\upsilon & ::= & \tt{a} | \tt{b} | \tt{c} | ... | \tt{aa} | ... &&\\ \eta & ::= & \tt{0} | \tt{1} | \tt{2} | ... | \tt{42} | ...\end{matrix}" /></a>

and types:

<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}&space;\sigma&space;&&space;::=&space;&&space;\beta&space;\\&space;&&space;|&space;&&space;\beta&space;\tt{\rightarrow}&space;\sigma&space;\\&space;&|&&space;\Pi\upsilon:\sigma.\sigma\\&space;\\&space;\beta&::=&&space;\sigma\,{\tt&space;space}\,&space;\upsilon&space;\\&space;&|&\gamma\\\\&space;\gamma&space;&&space;::=&&space;\tt{(}&space;\sigma&space;\tt{)}\\&space;&&space;|&&space;{\tt&space;Nat}\\&space;&|&&space;{\tt&space;Vec}\\&space;\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}&space;\sigma&space;&&space;::=&space;&&space;\beta&space;\\&space;&&space;|&space;&&space;\beta&space;\tt{\rightarrow}&space;\sigma&space;\\&space;&|&&space;\Pi\upsilon:\sigma.\sigma\\&space;\\&space;\beta&::=&&space;\sigma\,{\tt&space;space}\,&space;\upsilon&space;\\&space;&|&\gamma\\\\&space;\gamma&space;&&space;::=&&space;\tt{(}&space;\sigma&space;\tt{)}\\&space;&&space;|&&space;{\tt&space;Nat}\\&space;&|&&space;{\tt&space;Vec}\\&space;\end{matrix}" title="\begin{matrix} \sigma & ::= & \beta \\ & | & \beta \tt{\rightarrow} \sigma \\ &|& \Pi\upsilon:\sigma.\sigma\\ \\ \beta&::=& \sigma\,{\tt space}\, \upsilon \\ &|&\gamma\\\\ \gamma & ::=& \tt{(} \sigma \tt{)}\\ & |& {\tt Nat}\\ &|& {\tt Vec}\\ \end{matrix}" /></a>


Some notes about the syntax:

- The above syntax only covers the core calculus, and not the repl extensions (such as let bindings above). The extensions are simply added on in the repl.
- Term variables are strings (excluding numbers), as this is isomorphic to a whiteboard treatment and hence the most familiar. Term variables are lower case.
- Terms are variables, functions `\x:Nat.x`, applications `a b` (where `a` and `b` are in scope), empty lists `nil`, non-empty lists `cons 0 42 nil` which is a singleton list containing 42 of type `Vec Nat 1`, or a non-negative natural number `0,1,2,3,...`.
- Types are abstractions `Pi n:Nat.T`, applications `A t` (where `t` is a term), arrows `A -> B` (which is the same as a `Pi` type where the variable doesn't occur in `B`), `Nat`s, or size bounded vectors `Vec Nat t` where `t` is a term of type `Nat`.
- kinds are not part of the syntax, but play a role in typing. See `K`.
- Type arrows associate to the right so that `X -> Y -> Z` is the same as `X -> (Y -> Z)` but not `((X -> Y) -> Z)`.
- The Pi type binds weaker than arrows, so `Pi x:Nat. Nat->Nat` is the same as `Pi x:Nat. (Nat->Nat)`. 
- Nested terms don't need brackets: `\x:Nat.\y:Nat. y` unless enforcing application on the right. Whitespace does not matter `(\x:Nat.          x)` unless it is between application where you need at least one space. Like term-level functions, Pi must be put in brackets when applied to a term: `\x:(Pi n:Nat.Vec Nat n) 0.x`.
- To quit use `Ctrl+C` or whatever your machine uses to interrupt computations.

## Semantics

The semantics implements beta-reduction on terms and typed definitional equality as the `Eq` instance of `T`. The term semantics are the same as STLC with the addition of dependent abstraction of types over terms, term-dependent vectors and natural numbers. We reformulate the semantics as [typing judgements](https://existentialtype.wordpress.com/2011/03/27/the-holy-trinity/):

Firstly, we have the standard rule for variables augmented with kinding information (variables must be of [proper type](https://en.wikipedia.org/wiki/Type_constructor)): 

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{v&space;:&space;T&space;::&space;*&space;\in\Gamma}{\Gamma&space;\vdash&space;v:T}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{v&space;:&space;T&space;::&space;*&space;\in\Gamma}{\Gamma&space;\vdash&space;v:T}" title="\frac{v : T :: * \in\Gamma}{\Gamma \vdash v:T}" /></a>

For term abstractions we replace `->` with a dependent `Π` type, which allows the type on the right-hand side of the type to depend on the value passed on the left (`Πn:Nat.Vec Nat n` for instance). When the right-hand side does not use the variable:

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{\Gamma&space;\vdash&space;T_1&space;::&space;*\quad&space;\Gamma&space;,x:T_1&space;\vdash&space;T_2}{\Gamma&space;\vdash&space;\lambda&space;x:T_1.M&space;:\,&space;\Pi&space;x:T_1.T_2}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{\Gamma&space;\vdash&space;T_1&space;::&space;*\quad&space;\Gamma&space;,x:T_1&space;\vdash&space;T_2}{\Gamma&space;\vdash&space;\lambda&space;x:T_1.M&space;:\,&space;\Pi&space;x:T_1.T_2}" title="\frac{\Gamma \vdash T_1 :: *\quad \Gamma ,x:T_1 \vdash T_2}{\Gamma \vdash \lambda x:T_1.M :\, \Pi x:T_1.T_2}" /></a>

For term applications, we can now instantiate a type with a term provided to the application. The left-hand `Π` type must be parametrized by the same type as the value on the right-hand side of the application, and the value can then be substituted into the type. For instance in `(\n:Nat.cons n) 2 42`, `n` and 2 are of the same type and so the resultant expression has type `Vec Nat 2->Vec Nat 3` (see the typing rules below for nats.)

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{\Gamma&space;\vdash&space;f&space;:&space;\Pi&space;t:T_1&space;.&space;T_2\quad&space;\Gamma\,\vdash\,x&space;:&space;T_1}{\Gamma&space;\vdash&space;f\,x:T_2&space;[t&space;:=&space;x]}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{\Gamma&space;\vdash&space;f&space;:&space;\Pi&space;t:T_1&space;.&space;T_2\quad&space;\Gamma\,\vdash\,x&space;:&space;T_1}{\Gamma&space;\vdash&space;f\,x:T_2&space;[t&space;:=&space;x]}" title="\frac{\Gamma \vdash f : \Pi t:T_1 . T_2\quad \Gamma\,\vdash\,x : T_1}{\Gamma \vdash f\,x:T_2 [t := x]}" /></a>


We have the standard beta reduction from the untyped calculus at the term-level:

<a href="https://www.codecogs.com/eqnedit.php?latex=(\lambda&space;x&space;:&space;T&space;.&space;M)\,N&space;\rightsquigarrow&space;M&space;[x&space;:=&space;N]" target="_blank"><img src="https://latex.codecogs.com/gif.latex?(\lambda&space;x&space;:&space;T&space;.&space;M)\,N&space;\rightsquigarrow&space;M&space;[x&space;:=&space;N]" title="(\lambda x : T . M)\,N \rightsquigarrow M [x := N]" /></a>

We have standard typing rules for Nats (with standard reduction rules under terms omitted):

<a href="https://www.codecogs.com/eqnedit.php?latex=\overline{\Gamma&space;\vdash&space;n:Nat}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\overline{\Gamma&space;\vdash&space;n:Nat}" title="\overline{\Gamma \vdash n:Nat}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{\Gamma&space;\vdash&space;n&space;:&space;Nat}{\Gamma&space;\vdash&space;{\tt&space;succ}\,&space;n&space;:&space;Nat}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{\Gamma&space;\vdash&space;n&space;:&space;Nat}{\Gamma&space;\vdash&space;{\tt&space;succ}\,&space;n&space;:&space;Nat}" title="\frac{\Gamma \vdash n : Nat}{\Gamma \vdash {\tt succ}\, n : Nat}" /></a>

or put another way (see `typeof` in LF.hs):

<a href="https://www.codecogs.com/eqnedit.php?latex=\overline{\Gamma&space;\vdash&space;{\tt&space;succ}:&space;\Pi&space;\_:Nat.&space;Nat}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\overline{\Gamma&space;\vdash&space;{\tt&space;succ}:&space;\Pi&space;\_:Nat.&space;Nat}" title="\overline{\Gamma \vdash {\tt succ}: \Pi \_:Nat. Nat}" /></a>

where "Π _:Nat.Nat" is pretty printed as `Nat -> Nat` since `_` does not occur in the right-hand side of the type. 

Similarly we have typing rules for size-bounded Vectors. Since this is the first-order dependent types, we do not have type abstraction as the intention of this language is to show off dependent types in action. As a result, Vector types are parametrised only by Nats:

<a href="https://www.codecogs.com/eqnedit.php?latex=\overline{\Gamma&space;\vdash&space;{\tt&space;nil}:&space;Vec\,Nat\,0}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\overline{\Gamma&space;\vdash&space;{\tt&space;nil}:&space;Vec\,Nat\,0}" title="\overline{\Gamma \vdash {\tt nil}: Vec\,Nat\,0}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\overline{\Gamma&space;\vdash&space;{\tt&space;cons}:\,&space;\Pi\,n&space;:&space;Nat&space;.&space;Nat\rightarrow\,Vec\,Nat\,n&space;\rightarrow&space;Vec\,&space;Nat\,({\tt&space;succ}\,&space;n)}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\overline{\Gamma&space;\vdash&space;{\tt&space;cons}:\,&space;\Pi\,n&space;:&space;Nat&space;.&space;Nat\rightarrow\,Vec\,Nat\,n&space;\rightarrow&space;Vec\,&space;Nat\,({\tt&space;succ}\,&space;n)}" title="\overline{\Gamma \vdash {\tt cons}:\, \Pi\,n : Nat . Nat\rightarrow\,Vec\,Nat\,n \rightarrow Vec\, Nat\,({\tt succ}\, n)}" /></a>

We now have term-dependent abstraction (Pi type), application, and type-constants (like Nat) at the type level with 'types of types' known as kinds and type families. Type families are kinds indexed by a term, which is also indexing types of this kind as a result of the term application rule above:

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
# Simply Typed Lambda Calculus
Haskell implementation on Alonzo Church's untyped lambda calculus. It has a base type `O` and function type `T->T` to eliminate untypeable and paradoxical terms.

This calculus is of intense [foundational](https://existentialtype.wordpress.com/2011/03/27/the-holy-trinity/) interest and builds on ULC by prohibiting untypeable/paradoxical terms such as `\x.x x`.

## Prerequisites
You need [Haskell](https://www.haskell.org/), this compiles with GHC 8.2.2 at least (Stack resolver: lts-11.0).

_Optional_: If you want to run the tests for this module, you'll need [QuickCheck](http://hackage.haskell.org/package/QuickCheck-2.12.6.1/docs/Test-QuickCheck.html#v:label).

## To Build & Run

To compile and run do:
`ghc -O2 -o stlc Main.hs`
then run `./stlc`

Alternatively to use the GHCi Interpreter do:
`ghci Main`
then type `main`

In either case you get something like the following:
```
Welcome to the Simply Typed λ-calculus REPL
Type some terms or press Enter to leave.
>
```
Note: When run in GHCi, you don't have the luxury of escaped characters, backspace, delete etc...
Compile it using GHC if you need this.

## Examples 
Where you can then have some fun, try these examples:
- `\x:O.x`
- `\f:O->O.\x:O.f x`

The parser is also smart enough to recognise λ, so you can copy and paste from the output:
```
Welcome to the Simply Typed λ-calculus REPL
Type some terms or press Enter to leave.
>   \x:O.x
=   λx:O.x
>   λx:O.x
=   λx:O.x
```
`>` denotes the REPL waiting for input, `=` means no reductions occurred (it's the same term), `~>` denotes one reduction, and `~>*` denotes 0 or more reductions (although in practice this is 1 or more due to `=`).

There is also a reduction tracer, which should print each reduction step. prefix any string with `'` in order to see the reductions:
```
>   '\a:O.(\x:O.x) ((\y:O.y) a)
~>  λa:O.(λy:O.y) a
~>  λa:O.a
```

There is also a typing mechanism, which should display the type or fail as usual.
```
>   t\x:O->O.x x
Cannot Type Term: \x:O->O.x x
>   t\x:O.x
O->O
```

Note: if you provide a non-normalizing term, the type checker will fail and reduction will not occur.

You can save variables for the life of the program with a `let` expression. Any time a saved variable appears in a term, it will be substituted for the saved term:
```
>   let x = \y:O.y
Saved: λy:O.y
>   \z:O.x
=   λz:O.λy:O.y
```
Note: Consequently `let` and `=` are keywords, and so you cannot name variables with these.

## Syntax 

We base the language on the BNF for the typed calculus:

<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}&space;\mathbf{\tau}&&space;::=&space;&&space;\lambda&space;\mathbf{\upsilon}\tt{:}\sigma&space;.&space;\mathbf{\tau}\\&space;&&space;|&space;&&space;\tau\,&space;{\tt&space;space}\,&space;\tau\\&space;&&space;|&space;&&space;\upsilon&space;\\&space;&&\\&space;\upsilon&space;&&space;::=&space;&&space;\tt{a}&space;|&space;\tt{b}&space;|&space;\tt{c}&space;|&space;...&space;|&space;\tt{aa}&space;|&space;...&space;\\&space;&&\\&space;\sigma&space;&&space;::=&space;&&space;{\tt&space;O}\\&space;&&space;|&space;&&space;\sigma&space;\rightarrow&space;\sigma&space;\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}&space;\mathbf{\tau}&&space;::=&space;&&space;\lambda&space;\mathbf{\upsilon}\tt{:}\sigma&space;.&space;\mathbf{\tau}\\&space;&&space;|&space;&&space;\tau\,&space;{\tt&space;space}\,&space;\tau\\&space;&&space;|&space;&&space;\upsilon&space;\\&space;&&\\&space;\upsilon&space;&&space;::=&space;&&space;\tt{a}&space;|&space;\tt{b}&space;|&space;\tt{c}&space;|&space;...&space;|&space;\tt{aa}&space;|&space;...&space;\\&space;&&\\&space;\sigma&space;&&space;::=&space;&&space;{\tt&space;O}\\&space;&&space;|&space;&&space;\sigma&space;\rightarrow&space;\sigma&space;\end{matrix}" title="\begin{matrix} \mathbf{\tau}& ::= & \lambda \mathbf{\upsilon}\tt{:}\sigma . \mathbf{\tau}\\ & | & \tau\, {\tt space}\, \tau\\ & | & \upsilon \\ &&\\ \upsilon & ::= & \tt{a} | \tt{b} | \tt{c} | ... | \tt{aa} | ... \\ &&\\ \sigma & ::= & {\tt O}\\ & | & \sigma \rightarrow \sigma \end{matrix}" /></a>

However we adopt standard bracketing conventions to eliminate ambiguity in the parser. Concretely, the parser implements the non-ambiguous grammar as follows:

<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}&space;&&\\&space;\mathbf{\tau}&&space;::=&space;&&space;\lambda&space;\mathbf{\upsilon}\tt{:}\sigma&space;.&space;\mathbf{\tau}\\&space;&&space;|&space;&&space;\alpha\\&space;&&\\&space;\alpha&space;&&space;::=&space;&&space;\beta&space;\\&space;&|&space;&\mathbf{\alpha\,&space;\tt{space}\,&space;\beta}&space;\\&space;&&\\&space;\beta&space;&&space;::=&space;&&space;\tt{(}\tau&space;\tt{)}\\&space;&|&&space;\upsilon&space;\\&space;&&\\&space;\upsilon&space;&&space;::=&space;&&space;\tt{a}&space;|&space;\tt{b}&space;|&space;\tt{c}&space;|&space;...&space;|&space;\tt{aa}&space;|&space;...&space;&&\\&space;\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}&space;&&\\&space;\mathbf{\tau}&&space;::=&space;&&space;\lambda&space;\mathbf{\upsilon}\tt{:}\sigma&space;.&space;\mathbf{\tau}\\&space;&&space;|&space;&&space;\alpha\\&space;&&\\&space;\alpha&space;&&space;::=&space;&&space;\beta&space;\\&space;&|&space;&\mathbf{\alpha\,&space;\tt{space}\,&space;\beta}&space;\\&space;&&\\&space;\beta&space;&&space;::=&space;&&space;\tt{(}\tau&space;\tt{)}\\&space;&|&&space;\upsilon&space;\\&space;&&\\&space;\upsilon&space;&&space;::=&space;&&space;\tt{a}&space;|&space;\tt{b}&space;|&space;\tt{c}&space;|&space;...&space;|&space;\tt{aa}&space;|&space;...&space;&&\\&space;\end{matrix}" title="\begin{matrix} &&\\ \mathbf{\tau}& ::= & \lambda \mathbf{\upsilon}\tt{:}\sigma . \mathbf{\tau}\\ & | & \alpha\\ &&\\ \alpha & ::= & \beta \\ &| &\mathbf{\alpha\, \tt{space}\, \beta} \\ &&\\ \beta & ::= & \tt{(}\tau \tt{)}\\ &|& \upsilon \\ &&\\ \upsilon & ::= & \tt{a} | \tt{b} | \tt{c} | ... | \tt{aa} | ... &&\\ \end{matrix}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}\sigma&space;&&space;::=&space;&&space;\gamma&space;\\&space;&&space;|&space;&&space;\gamma&space;\tt{\rightarrow}&space;\sigma&space;\\&space;&&\\&space;\gamma&space;&&space;::=&&space;\tt{(}&space;\sigma&space;\tt{)}\,&space;|\,&space;\tt{O}&space;\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}\sigma&space;&&space;::=&space;&&space;\gamma&space;\\&space;&&space;|&space;&&space;\gamma&space;\tt{\rightarrow}&space;\sigma&space;\\&space;&&\\&space;\gamma&space;&&space;::=&&space;\tt{(}&space;\sigma&space;\tt{)}\,&space;|\,&space;\tt{O}&space;\end{matrix}" title="\begin{matrix}\sigma & ::= & \gamma \\ & | & \gamma \tt{\rightarrow} \sigma \\ &&\\ \gamma & ::=& \tt{(} \sigma \tt{)}\, |\, \tt{O} \end{matrix}" /></a>

Some notes about the syntax:

- The above syntax only covers the core calculus, and not the repl extensions (such as let bindings above). The extensions are simply added on in the repl.
- Variables are strings (excluding numbers), as this is isomorphic to a whiteboard treatment and hence the most familiar.
- Types are either literal `O` base types or nested arrow types: `O -> O`. Arrows associate to the right so that `O -> O -> O` is the same as `O -> (O -> O)` but not `((O -> O) -> O)`. Alternative implementations let O range over a set of base types (like int, float etc...) but this is semantically equivalent unless we care about those types.
- Nested terms don't need brackets: `\x:O.\y:O. y` unless enforcing application on the right. Whitespace does not matter `(\x:O.          x)` unless it is between application where you need at least one space.
- To quit use `Ctrl+C` or whatever your machine uses to interrupt computations.

## Semantics

The semantics implements beta-reduction on terms and alpha-equivalence as the `Eq` instance of `STTerm`. The semantics are the same as the untyped calculus with the addition of types. We reformulate the semantics as [typing judgements](https://existentialtype.wordpress.com/2011/03/27/the-holy-trinity/):

for variables:

<a href="https://www.codecogs.com/eqnedit.php?latex=\overline{\Gamma&space;\vdash&space;x:T},\quad&space;\mbox{(if&space;$x:T&space;\in&space;\Gamma$)}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\overline{\Gamma&space;\vdash&space;x:T},\quad&space;\mbox{(if&space;$x:T&space;\in&space;\Gamma$)}" title="\overline{\Gamma \vdash x:T},\quad \mbox{(if $x:T \in \Gamma$)}" /></a>

for abstractions:

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{\Gamma&space;,x:A&space;\vdash&space;t:B}{\Gamma&space;\vdash&space;(\lambda&space;x&space;:&space;A.&space;t)&space;:&space;B&space;}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{\Gamma&space;,x:A&space;\vdash&space;t:B}{\Gamma&space;\vdash&space;(\lambda&space;x&space;:&space;A.&space;t)&space;:&space;B&space;}" title="\frac{\Gamma ,x:A \vdash t:B}{\Gamma \vdash (\lambda x : A. t) : B }" /></a>

and application:

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{\Gamma&space;\vdash&space;f&space;:&space;A&space;\Rightarrow&space;B\quad&space;\Gamma&space;\vdash&space;x&space;:&space;A}{\Gamma&space;\vdash&space;(f&space;x)&space;:&space;B}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{\Gamma&space;\vdash&space;f&space;:&space;A&space;\Rightarrow&space;B\quad&space;\Gamma&space;\vdash&space;x&space;:&space;A}{\Gamma&space;\vdash&space;(f&space;x)&space;:&space;B}" title="\frac{\Gamma \vdash f : A \Rightarrow B\quad \Gamma \vdash x : A}{\Gamma \vdash (f x) : B}" /></a>

and the reduction relation adopted from the untyped theory (with types added in the abstraction):

<a href="https://www.codecogs.com/eqnedit.php?latex=(\lambda&space;x&space;:&space;T&space;.&space;M)N&space;\rightsquigarrow&space;M&space;[x&space;:=&space;N]" target="_blank"><img src="https://latex.codecogs.com/gif.latex?(\lambda&space;x&space;:&space;T&space;.&space;M)N&space;\rightsquigarrow&space;M&space;[x&space;:=&space;N]" title="(\lambda x : T . M)N \rightsquigarrow M [x := N]" /></a>

- This implementation follows a [small-step](https://cs.stackexchange.com/questions/43294/difference-between-small-and-big-step-operational-semantics) operational semantics and Berendregt's [variable convention](https://cs.stackexchange.com/questions/69323/barendregts-variable-convention-what-does-it-mean) (see `substitution` in STLC.hs). 
- Reductions include the one-step reduction (see `reduce1` in STLC.hs), the many-step reduction (see `reduce` in STLC.hs). 

## Other Implementation Details
- STLC.hs contains the Haskell implementation of the calculus, including substitution, reduction, and other useful things.
- Parser.hs contains the monadic parser combinators needed to parse input strings into typed-term ASTs for the calculus.
- Repl.hs contains a simple read-eval-print loop which hooks into main, and into the parser.
- Main.hs is needed for GHC to compile without any flags, it also invokes the repl.
- Tests.hs is the test suite. We have unit tests for terms in the language. QuickCheck is used to generate arbitrary trees and test they are parsed and printed correctly.

For contributions, see the [project to-do list](https://github.com/lukeg101/lplzoo/projects/2) or submit a PR with something you think it needs.

Work initially documented [here](https://gist.github.com/lukeg101/b3b305ac9438d1a57a0669f81cb0bab2).



# System T
Haskell implementation on Kurt Godel's typed lambda calculus. It has a base type `Nat`, `Succ`essors on Nats, the function type `T->T`, and primitive recursion on Nats. It is strongly normalizing, but not Turing Complete.

Godel used this language to prove the consistency of arithmetic.

## Prerequisites
You need [Haskell](https://www.haskell.org/), this compiles with GHC 8.2.2 at least (Stack resolver: lts-11.0).

## To Build & Run

You can use cabal to build and run this, see this [README](../README.md), alternatively you can use vanilla ghc to build:

To compile and run do:
`ghc -O2 -o st Main.hs`
then run `./st`

Alternatively to use the GHCi Interpreter do:
`ghci Main`
then type `main`

In either case you get something like the following:
```
Welcome to the System T REPL
Type some terms or press Enter to leave.
>
```

Note: When run in GHCi, you don't have the luxury of escaped characters, backspace, delete etc...
Compile it using GHC if you need this.

## Examples 
Where you can then have some fun, try these examples:
- `\x:Nat.x`
- `s (s z)`

The parser is also smart enough to recognise λ, so you can copy and paste from the output:
```
Welcome to the System T REPL
Type some terms or press Enter to leave.
>   \x:Nat.x
=   λx:Nat.x
>   λx:Nat.x
=   λx:Nat.x
```
`>` denotes the REPL waiting for input, `=` means no reductions occurred (it's the same term), `~>` denotes one reduction, and `~>*` denotes 0 or more reductions (although in practice this is 1 or more due to `=`).

There is also a reduction tracer, which should print each reduction step. prefix any string with `'` in order to see the reductions:
```
>   '(λa:Nat.λb:Nat.rec (λc:Nat.λd:Nat.s d) a b) z (s z)
~>  (λb:Nat.rec (λc:Nat.λd:Nat.s d) z b) (s z)
~>  rec (λc:Nat.λd:Nat.s d) z (s z)
~>  (λc:Nat.λd:Nat.s d) z (rec (λc:Nat.λd:Nat.s d) z z)
~>  (λd:Nat.s d) (rec (λc:Nat.λd:Nat.s d) z z)
~>  s (rec (λc:Nat.λd:Nat.s d) z z)
~>  s z
```
Note: The above adds zero to one.

There is also a typing mechanism, which should display the type or fail as usual.
```
>   t(λx:Nat.λy:Nat.rec (λa:Nat.λb:Nat.s b) x y) z (s z)
Nat
>   t\x:Nat. x x
Cannot Type Term: \x:Nat. x x
```

Note: if you provide a non-normalizing term, the type checker will fail and reduction will not occur.

You can save variables for the life of the program with a `let` expression. Any time a saved variable appears in a term, it will be substituted for the saved term:
```
>   let one = s z
Saved: s z
>   let plus = λa:Nat.λb:Nat.rec (λc:Nat.λd:Nat.s d) a b
Saved: λa:Nat.λb:Nat.rec (λc:Nat.λd:Nat.s d) a b
>   plus one one
~>* s (s z)

```
Note: Consequently `let` and `=` are keywords, and so you cannot name variables with these. Additionally `z`, `s`, and `rec` are keywords in System T.

## Syntax 

We base the language on the BNF for System T:

<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}&space;\mathbf{\tau}&&space;::=&space;&&space;\lambda&space;\mathbf{\upsilon}{\tt&space;:}\sigma&space;.&space;\mathbf{\tau}\\&space;&&space;|&space;&&space;\tau\,&space;\tau\\&space;&&space;|&space;&&space;\upsilon&space;\\&space;&|&&space;s\,\tau\\&space;&|&&space;z&space;\\&space;&|&&space;rec\,\tau\,\tau\,\tau\\&space;&&\\&space;\upsilon&space;&&space;::=&space;&&space;\tt{a}&space;|&space;\tt{b}&space;|&space;\tt{c}&space;|&space;...&space;|&space;\tt{aa}&space;|&space;...&space;\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}&space;\mathbf{\tau}&&space;::=&space;&&space;\lambda&space;\mathbf{\upsilon}{\tt&space;:}\sigma&space;.&space;\mathbf{\tau}\\&space;&&space;|&space;&&space;\tau\,&space;\tau\\&space;&&space;|&space;&&space;\upsilon&space;\\&space;&|&&space;s\,\tau\\&space;&|&&space;z&space;\\&space;&|&&space;rec\,\tau\,\tau\,\tau\\&space;&&\\&space;\upsilon&space;&&space;::=&space;&&space;\tt{a}&space;|&space;\tt{b}&space;|&space;\tt{c}&space;|&space;...&space;|&space;\tt{aa}&space;|&space;...&space;\end{matrix}" title="\begin{matrix} \mathbf{\tau}& ::= & \lambda \mathbf{\upsilon}{\tt :}\sigma . \mathbf{\tau}\\ & | & \tau\, \tau\\ & | & \upsilon \\ &|& s\,\tau\\ &|& z \\ &|& rec\,\tau\,\tau\,\tau\\ &&\\ \upsilon & ::= & \tt{a} | \tt{b} | \tt{c} | ... | \tt{aa} | ... \end{matrix}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}&space;\sigma&space;&&space;::=&space;&&space;{\tt&space;Nat}\\&space;&&space;|&space;&&space;\sigma&space;\rightarrow&space;\sigma&space;\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}&space;\sigma&space;&&space;::=&space;&&space;{\tt&space;Nat}\\&space;&&space;|&space;&&space;\sigma&space;\rightarrow&space;\sigma&space;\end{matrix}" title="\begin{matrix} \sigma & ::= & {\tt Nat}\\ & | & \sigma \rightarrow \sigma \end{matrix}" /></a>

However we adopt standard bracketing conventions to eliminate ambiguity in the parser. Concretely, the parser implements the non-ambiguous grammar as follows:

<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}&space;&&\\&space;\mathbf{\tau}&&space;::=&space;&&space;\lambda&space;\mathbf{\upsilon}\tt{:}\sigma&space;.&space;\mathbf{\tau}\\&space;&&space;|&space;&&space;\alpha\\&space;&&\\&space;\alpha&space;&&space;::=&space;&&space;\beta&space;\\&space;&|&space;&\mathbf{\alpha\,&space;\tt{space}\,&space;\beta}&space;\\&space;&&\\&space;\beta&space;&&space;::=&space;&&space;\tt{(}\tau&space;\tt{)}\\&space;&|&&space;\upsilon&space;\\&space;&|&\tt{z}\\&space;&|&\tt{s}\\&space;&|&\tt{rec}\\&space;&&\\&space;\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}&space;&&\\&space;\mathbf{\tau}&&space;::=&space;&&space;\lambda&space;\mathbf{\upsilon}\tt{:}\sigma&space;.&space;\mathbf{\tau}\\&space;&&space;|&space;&&space;\alpha\\&space;&&\\&space;\alpha&space;&&space;::=&space;&&space;\beta&space;\\&space;&|&space;&\mathbf{\alpha\,&space;\tt{space}\,&space;\beta}&space;\\&space;&&\\&space;\beta&space;&&space;::=&space;&&space;\tt{(}\tau&space;\tt{)}\\&space;&|&&space;\upsilon&space;\\&space;&|&\tt{z}\\&space;&|&\tt{s}\\&space;&|&\tt{rec}\\&space;&&\\&space;\end{matrix}" title="\begin{matrix} &&\\ \mathbf{\tau}& ::= & \lambda \mathbf{\upsilon}\tt{:}\sigma . \mathbf{\tau}\\ & | & \alpha\\ &&\\ \alpha & ::= & \beta \\ &| &\mathbf{\alpha\, \tt{space}\, \beta} \\ &&\\ \beta & ::= & \tt{(}\tau \tt{)}\\ &|& \upsilon \\ &|&\tt{z}\\ &|&\tt{s}\\ &|&\tt{rec}\\ &&\\ \end{matrix}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=&&\\&space;\upsilon&space;&&space;::=&space;&&space;\tt{a}&space;|&space;\tt{b}&space;|&space;\tt{c}&space;|&space;...&space;|&space;\tt{aa}&space;|&space;...&space;\\" target="_blank"><img src="https://latex.codecogs.com/gif.latex?&&\\&space;\upsilon&space;&&space;::=&space;&&space;\tt{a}&space;|&space;\tt{b}&space;|&space;\tt{c}&space;|&space;...&space;|&space;\tt{aa}&space;|&space;...&space;\\" title="&&\\ \upsilon & ::= & \tt{a} | \tt{b} | \tt{c} | ... | \tt{aa} | ... \\" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}&space;\sigma&space;&&space;::=&space;&&space;\gamma&space;\\&space;&&space;|&space;&&space;\gamma&space;\tt{\rightarrow}&space;\sigma&space;\\&space;&&\\&space;\gamma&space;&&space;::=&&space;\tt{(}&space;\sigma&space;\tt{)}\,&space;|\,&space;\tt{Nat}&space;\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}&space;\sigma&space;&&space;::=&space;&&space;\gamma&space;\\&space;&&space;|&space;&&space;\gamma&space;\tt{\rightarrow}&space;\sigma&space;\\&space;&&\\&space;\gamma&space;&&space;::=&&space;\tt{(}&space;\sigma&space;\tt{)}\,&space;|\,&space;\tt{Nat}&space;\end{matrix}" title="\begin{matrix} \sigma & ::= & \gamma \\ & | & \gamma \tt{\rightarrow} \sigma \\ &&\\ \gamma & ::=& \tt{(} \sigma \tt{)}\, |\, \tt{Nat} \end{matrix}" /></a>

Some notes about the syntax:

- The above syntax only covers the core calculus, and not the repl extensions (such as let bindings above). The extensions are simply added on in the repl.
- Variables are strings (excluding numbers), as this is isomorphic to a whiteboard treatment and hence the most familiar.
- Types are either literal `Nat` base types or nested arrow types: `T -> T`. Arrows associate to the right so that `Nat -> Nat -> Nat` is the same as `Nat -> (Nat -> Nat)` but not `((Nat -> Nat) -> Nat)`.
- Nested terms don't need brackets: `\x:Nat.\y:Nat. x` unless enforcing application on the right. Whitespace does not matter `(\x:Nat.          x)` unless it is between application where you need at least one space.
- To quit use `Ctrl+C` or whatever your machine uses to interrupt computations.

## Semantics

The semantics implements beta-reduction on terms and alpha-equivalence as the `Eq` instance of `STTerm`. The semantics are the same as the STLC but with additional rules for zero, succ, and rec. We reformulate the semantics as [typing judgements](https://existentialtype.wordpress.com/2011/03/27/the-holy-trinity/):

for variables

<a href="https://www.codecogs.com/eqnedit.php?latex=\overline{\Gamma&space;\vdash&space;x:T},\quad&space;\mbox{(if&space;$x:T&space;\in&space;\Gamma$)}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\overline{\Gamma&space;\vdash&space;x:T},\quad&space;\mbox{(if&space;$x:T&space;\in&space;\Gamma$)}" title="\overline{\Gamma \vdash x:T},\quad \mbox{(if $x:T \in \Gamma$)}" /></a>

for abstractions:

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{\Gamma&space;,x:A&space;\vdash&space;t:B}{\Gamma&space;\vdash&space;(\lambda&space;x&space;:&space;A.&space;t)&space;:&space;B&space;}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{\Gamma&space;,x:A&space;\vdash&space;t:B}{\Gamma&space;\vdash&space;(\lambda&space;x&space;:&space;A.&space;t)&space;:&space;B&space;}" title="\frac{\Gamma ,x:A \vdash t:B}{\Gamma \vdash (\lambda x : A. t) : B }" /></a>

and application:

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{\Gamma&space;\vdash&space;f&space;:&space;A&space;\Rightarrow&space;B\quad&space;\Gamma&space;\vdash&space;x&space;:&space;A}{\Gamma&space;\vdash&space;(f&space;x)&space;:&space;B}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{\Gamma&space;\vdash&space;f&space;:&space;A&space;\Rightarrow&space;B\quad&space;\Gamma&space;\vdash&space;x&space;:&space;A}{\Gamma&space;\vdash&space;(f&space;x)&space;:&space;B}" title="\frac{\Gamma \vdash f : A \Rightarrow B\quad \Gamma \vdash x : A}{\Gamma \vdash (f x) : B}" /></a>

the reduction relation is adopted from STLC:

<a href="https://www.codecogs.com/eqnedit.php?latex=(\lambda&space;x&space;:&space;T&space;.&space;M)N&space;\rightsquigarrow&space;M&space;[x&space;:=&space;N]" target="_blank"><img src="https://latex.codecogs.com/gif.latex?(\lambda&space;x&space;:&space;T&space;.&space;M)N&space;\rightsquigarrow&space;M&space;[x&space;:=&space;N]" title="(\lambda x : T . M)N \rightsquigarrow M [x := N]" /></a>

for zero and succ:

<a href="https://www.codecogs.com/eqnedit.php?latex=\overline{\Gamma&space;\vdash&space;z:Nat}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\overline{\Gamma&space;\vdash&space;z:Nat}" title="\overline{\Gamma \vdash z:Nat}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{\Gamma&space;\vdash&space;n&space;:&space;Nat}{\Gamma&space;\vdash&space;s\,&space;n&space;:&space;Nat}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{\Gamma&space;\vdash&space;n&space;:&space;Nat}{\Gamma&space;\vdash&space;s\,&space;n&space;:&space;Nat}" title="\frac{\Gamma \vdash n : Nat}{\Gamma \vdash s\, n : Nat}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{n&space;\rightsquigarrow&space;n'}{s\,&space;n&space;\rightsquigarrow&space;s\,n'}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{n&space;\rightsquigarrow&space;n'}{s\,&space;n&space;\rightsquigarrow&space;s\,n'}" title="\frac{n \rightsquigarrow n'}{s\, n \rightsquigarrow s\,n'}" /></a>

There are similar _inner_ reduction rules for each of the inner arguments of `rec` however these are subsumed by beta-reduction. There are special elimination and reduction rules for primitive recursion on Nats:

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{\Gamma&space;\vdash&space;h&space;:&space;Nat&space;\rightarrow&space;T&space;\rightarrow&space;T\quad&space;\Gamma&space;\vdash&space;a&space;:&space;T\quad&space;\Gamma&space;\vdash&space;n:Nat&space;}&space;{\Gamma&space;\vdash&space;rec_{T}\,h\,a\,n&space;:&space;T}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{\Gamma&space;\vdash&space;h&space;:&space;Nat&space;\rightarrow&space;T&space;\rightarrow&space;T\quad&space;\Gamma&space;\vdash&space;a&space;:&space;T\quad&space;\Gamma&space;\vdash&space;n:Nat&space;}&space;{\Gamma&space;\vdash&space;rec_{T}\,h\,a\,n&space;:&space;T}" title="\frac{\Gamma \vdash h : Nat \rightarrow T \rightarrow T\quad \Gamma \vdash a : T\quad \Gamma \vdash n:Nat } {\Gamma \vdash rec_{T}\,h\,a\,n : T}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=rec_{T}\,h\,a\,z&space;\rightsquigarrow&space;a" target="_blank"><img src="https://latex.codecogs.com/gif.latex?rec_{T}\,h\,a\,z&space;\rightsquigarrow&space;a" title="rec_{T}\,h\,a\,z \rightsquigarrow a" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=rec_{T}\,h\,a\,(s\,&space;n)&space;\rightsquigarrow&space;h\,n\,(rec_{T}\,&space;h\,a\,n)" target="_blank"><img src="https://latex.codecogs.com/gif.latex?rec_{T}\,h\,a\,(s\,&space;n)&space;\rightsquigarrow&space;h\,n\,(rec_{T}\,&space;h\,a\,n)" title="rec_{T}\,h\,a\,(s\, n) \rightsquigarrow h\,n\,(rec_{T}\, h\,a\,n)" /></a>

- This implementation follows a [small-step](https://cs.stackexchange.com/questions/43294/difference-between-small-and-big-step-operational-semantics) operational semantics and Berendregt's [variable convention](https://cs.stackexchange.com/questions/69323/barendregts-variable-convention-what-does-it-mean) (see `substitution` in SystemT.hs). 
- Reductions include the one-step reduction (see `reduce1` in SystemT.hs), the many-step reduction (see `reduce` in SystemT.hs). 

## Other Implementation Details
- SystemT.hs contains the Haskell implementation of the calculus, including substitution, reduction, and other useful things.
- Parser.hs contains the monadic parser combinators needed to parse input strings into typed-term ASTs for the calculus.
- Repl.hs contains a simple read-eval-print loop which hooks into main, and into the parser.
- Main.hs is needed for GHC to compile without any flags, it also invokes the repl.
- Tests.hs is the test suite. We have unit tests for terms in the language. QuickCheck is used to generate arbitrary trees and test they are parsed and printed correctly.

For contributions, see the [project to-do list](https://github.com/lukeg101/lplzoo/projects/3) or submit a PR with something you think it needs.

Work initially documented [here](https://gist.github.com/lukeg101/17d0a70f6ffd61978bdbe97436509571).



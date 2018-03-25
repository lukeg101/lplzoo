# System T
Haskell implementation on Kurt Godel's typed lambda calculus. It has a base type `Nat`, `Succ`essors on Nats, the function type `T->T`, and primitive recursion on Nats.

## Prerequisites
You need [Haskell](https://www.haskell.org/), this compiles with GHC 8.2.2 at least (Stack resolver: lts-11.0).

## To Build & Run

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
- `\1:Nat.1`
- `s s z`

The parser is also smart enough to recognise λ, so you can copy and paste from the output:
```
> \1:Nat.1
λ1:Nat.1
> λ1:Nat.1
λ1:Nat.1
```

There is also a reduction tracer, which should print each reduction step. prefix any string with `'` in order to see the reductions:
```
> '(λ1:Nat.λ2:Nat.rec (λ3:Nat.λ4:Nat.s 4) 1 2) z (s z)
(λ2:Nat.rec (λ3:Nat.λ4:Nat.s 4) z 2) (s z)
rec (λ3:Nat.λ4:Nat.s 4) z (s z)
(λ3:Nat.λ4:Nat.s 4) z (rec (λ3:Nat.λ4:Nat.s 4) z z)
(λ4:Nat.s 4) (rec (λ3:Nat.λ4:Nat.s 4) z z)
s (rec (λ3:Nat.λ4:Nat.s 4) z z)
s z
```
Note: The above adds zero to one.

There is also a typing mechanism, which should display the type or fail as usual.
```
> t(λ1:Nat.λ2:Nat.rec (λ3:Nat.λ4:Nat.s 4) 1 2) z (s z)
Nat
> \1:Nat. 1 1
Cannot Type Term: \1:Nat. 1 1
```

Note: if you provide a non-normalizing term, the type checker will fail and reduction will not occur.

## Syntax 

The syntax for the parser follows the CFG for System T with the standard notational conventions for brackets. The full syntax is:

<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}&space;&&\\&space;\mathbf{\tau}&&space;::=&space;&&space;\lambda&space;\mathbf{\upsilon}\tt{:}\sigma&space;.&space;\mathbf{\tau}\\&space;&&space;|&space;&&space;\alpha\\&space;&&\\&space;\alpha&space;&&space;::=&space;&&space;\beta&space;\\&space;&|&space;&\mathbf{\alpha\,&space;\tt{space}\,&space;\beta}&space;\\&space;&|&&space;\tt{s}\,&space;\tt{space}\,\beta&space;\\&space;&|&\tt{rec}\,&space;\tt{space}\,\beta\,&space;\tt{space}\,\beta\,&space;\tt{space}\,\beta&space;\\&space;&&\\&space;\beta&space;&&space;::=&space;&&space;\tt{(}\tau&space;\tt{)}\\&space;&|&&space;\upsilon&space;\\&space;&|&z\\&&\\&space;\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}&space;&&\\&space;\mathbf{\tau}&&space;::=&space;&&space;\lambda&space;\mathbf{\upsilon}\tt{:}\sigma&space;.&space;\mathbf{\tau}\\&space;&&space;|&space;&&space;\alpha\\&space;&&\\&space;\alpha&space;&&space;::=&space;&&space;\beta&space;\\&space;&|&space;&\mathbf{\alpha\,&space;\tt{space}\,&space;\beta}&space;\\&space;&|&&space;\tt{s}\,&space;\tt{space}\,\beta&space;\\&space;&|&\tt{rec}\,&space;\tt{space}\,\beta\,&space;\tt{space}\,\beta\,&space;\tt{space}\,\beta&space;\\&space;&&\\&space;\beta&space;&&space;::=&space;&&space;\tt{(}\tau&space;\tt{)}\\&space;&|&&space;\upsilon&space;\\&space;&|&z\\&&\\&space;\end{matrix}" title="\begin{matrix} &&\\ \mathbf{\tau}& ::= & \lambda \mathbf{\upsilon}\tt{:}\sigma . \mathbf{\tau}\\ & | & \alpha\\ &&\\ \alpha & ::= & \beta \\ &| &\mathbf{\alpha\, \tt{space}\, \beta} \\ &|& \tt{s}\, \tt{space}\,\beta \\ &|&\tt{rec}\, \tt{space}\,\beta\, \tt{space}\,\beta\, \tt{space}\,\beta \\ &&\\ \beta & ::= & \tt{(}\tau \tt{)}\\ &|& \upsilon \\ &|&z\\&&\\ \end{matrix}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}&space;\sigma&space;&&space;::=&space;&&space;\gamma&space;\\&space;&&space;|&space;&&space;\gamma&space;\tt{\rightarrow}&space;\sigma&space;\\&space;&&\\&space;\gamma&space;&&space;::=&&space;\tt{(}&space;\sigma&space;\tt{)}\,&space;|\,&space;\tt{Nat}&space;\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}&space;\sigma&space;&&space;::=&space;&&space;\gamma&space;\\&space;&&space;|&space;&&space;\gamma&space;\tt{\rightarrow}&space;\sigma&space;\\&space;&&\\&space;\gamma&space;&&space;::=&&space;\tt{(}&space;\sigma&space;\tt{)}\,&space;|\,&space;\tt{Nat}&space;\end{matrix}" title="\begin{matrix} \sigma & ::= & \gamma \\ & | & \gamma \tt{\rightarrow} \sigma \\ &&\\ \gamma & ::=& \tt{(} \sigma \tt{)}\, |\, \tt{Nat} \end{matrix}" /></a>

It's possible this grammar is ambiguous; submit a PR if you check! Some notes about the syntax:

- Variables are positive integers (including zero) as this is easy for Haskell to process, and for me implement variable generation. This is isomorphic to a whiteboard treatment using characters (like `\x:Nat.x`).
- Types are either literal `Nat` base types or nested arrow types: `T -> T`. Arrows associate to the left so that `Nat -> Nat -> Nat` is the same as `((Nat -> Nat) -> Nat)` but not `Nat -> (Nat -> Nat)`.
- Nested terms don't need brackets: `\1:Nat.\2:Nat. 2` unless enforcing application on the right. Whitespace does not matter `(\1:Nat.          1)` unless it is between application where you need at least one space.
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

and there are special elimination and reduction rules for primitive recursion on Nats:

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

For contributions, see the [project to-do list](https://github.com/lukeg101/lplzoo/projects/3) or submit a PR with something you think it needs.

Work initially documented [here](https://gist.github.com/lukeg101/17d0a70f6ffd61978bdbe97436509571).



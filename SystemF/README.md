# System F
Haskell implementation on Jean Yves-Girard's System F. It has the features of STLC but with second-order abstraction over types, to enable parametric polymorphism.

This strongly normalising calculus was used to prove the consistency of mathematical analysis and later rediscovered by Reynolds as the paradigm supporting polymorphism.

## Prerequisites
You need [Haskell](https://www.haskell.org/), this compiles with GHC 8.2.2 at least (Stack resolver: lts-11.0).

## To Build & Run

To compile and run do:
`ghc -O2 -o sf Main`
then run `./sf`

Alternatively to use the GHCi Interpreter do:
`ghci Main`
then type `main`

In either case you get something like the following:
```
Welcome to the System F REPL
Type some terms or press Enter to leave.
>
```

Note: When run in GHCi, you don't have the luxury of escaped characters, backspace, delete etc...
Compile it using GHC if you need this.

## Examples 
Where you can then have some fun, try these examples:
- `L1.\2:1.2` where `L` stands for second-order abstraction.
- `(L2.\3:2->2.\4:2.4)` this is _zero_.
- `λ1:Π2.(2->2)->2->2.Λ2.λ3:2->2.λ4:2.3 (1 [2] 3 4)` this is _succ_

The parser is also smart enough to recognise λ and Λ, so you can copy and paste from the output:
```
Welcome to the System F REPL
Type some terms or press Enter to leave.
> L1.\2:1.2
Λ1.λ2:1.2
> Λ1.λ2:1.2
Λ1.λ2:1.2
```

There is also a reduction tracer, which should print each reduction step. prefix any string with `'` in order to see the reductions:
```
> '(λ1:Π2.(2->2)->2->2.Λ2.λ3:2->2.λ4:2.3 (1 [2] 3 4)) (L2.\3:2->2.\4:2.4)
Λ2.λ3:2->2.λ4:2.3 ((Λ2.λ3:2->2.λ4:2.4) [2] 3 4)
Λ2.λ3:2->2.λ4:2.3 ((λ3:2->2.λ4:2.4) 3 4)
Λ2.λ3:2->2.λ4:2.3 ((λ4:2.4) 4)
Λ2.λ3:2->2.λ4:2.3 4
```
Note: this is succ zero in Church Numeral format

There is also a typing mechanism, which should display the type or fail as usual.
```
> t(λ1:Π2.(2->2)->2->2.Λ2.λ3:2->2.λ4:2.3 (1 [2] 3 4)) (L2.\3:2->2.\4:2.4)
Π2.(2->2)->2->2
> tL1.\2:1. 2 2
Cannot Type Term: L1.\2:1. 2 2
>
```
where `Π2.(2->2)->2->2` is the System F type for Nats

Note: if you provide a non-normalizing term, the type checker will fail and reduction will not occur.

## Syntax 

We base the language on the BNF for the typed calculus:

TODO

However we adopt standard bracketing conventions to eliminate ambiguity in the parser. Concretely, the parser implements the non-ambiguous grammar as follows:

TODO

Some notes about the syntax:

- Variables are positive integers (including zero) as this is easy for Haskell to process, and for me implement variable generation. This is isomorphic to a whiteboard treatment using characters (like `\x:O.x`).
- Types are either literal `O` base types or nested arrow types: `O -> O`. Arrows associate to the right so that `O -> O -> O` is the same as `O -> (O -> O)` but not `((O -> O) -> O)`. Alternative implementations let O range over a set of base types (like int, float etc...) but this is semantically equivalent unless we care about those types.
- Nested terms don't need brackets: `\1:O.\2:O. 2` unless enforcing application on the right. Whitespace does not matter `(\1:O.          1)` unless it is between application where you need at least one space.
- To quit use `Ctrl+C` or whatever your machine uses to interrupt computations.

TODO

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

For contributions, see the [project to-do list](https://github.com/lukeg101/lplzoo/projects/2) or submit a PR with something you think it needs.

Work initially documented [here](https://gist.github.com/lukeg101/b3b305ac9438d1a57a0669f81cb0bab2).



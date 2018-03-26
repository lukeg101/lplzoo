# Programming Computable Functions
Haskell implementation on Gordon Plotkin's typed lambda calculus. It has a base type `Nat`, the function type `T->T`, `Succ` \\ `Pred`essors on Nats, and [general](https://stackoverflow.com/questions/1712237/how-does-primitive-recursion-differ-from-normal-recursion) recursion on Nats through use of a [Y combinator](https://en.wikipedia.org/wiki/Fixed-point_combinator#Fixed_point_combinators_in_lambda_calculus).

## Prerequisites
You need [Haskell](https://www.haskell.org/), this compiles with GHC 8.2.2 at least (Stack resolver: lts-11.0).

## To Build & Run

To compile and run do:
`ghc -O2 -o pcf Main`
then run `./pcf`

Alternatively to use the GHCi Interpreter do:
`ghci Main`
then type `main`

In either case you get something like the following:
```
Welcome to the PCF REPL
Type some terms or press Enter to leave.
>
```

Note: When run in GHCi, you don't have the luxury of escaped characters, backspace, delete etc...
Compile it using GHC if you need this.

## Examples 
Where you can then have some fun, try these examples:
- `\1:Nat.1`
- `p (s z)`

The parser is also smart enough to recognise λ, so you can copy and paste from the output:
```
> \1:Nat.1
λ1:Nat.1
> λ1:Nat.1
λ1:Nat.1
```

There is also a reduction tracer, which should print each reduction step. prefix any string with `'` in order to see the reductions:
```
> '(\1:Nat.\2:Nat. 1) z (s z)
(λ2:Nat.z) (s z)
z
```

There is also a typing mechanism, which should display the type or fail as usual.
```
> t(\1:Nat.\2:Nat. 1) z (s z)
Nat
> t(\1:Nat. 1 1)
Cannot Type Term: (\1:Nat. 1 1)
```

Note: if you provide a non-normalizing term (without Y), the type checker will fail and reduction will not occur.

Termination is not guaranteed if you misuse `Y`, just like in ULC.

## Syntax 

The syntax for the parser follows the CFG for PCF with the standard notational conventions for brackets. The full syntax is:

TODO

It's possible this grammar is ambiguous; submit a PR if you check! Some notes about the syntax:

- Variables are positive integers (including zero) as this is easy for Haskell to process, and for me implement variable generation. This is isomorphic to a whiteboard treatment using characters (like `\x:Nat.x`).
- Types are either literal `Nat` base types or nested arrow types: `T -> T`. Arrows associate to the left so that `Nat -> Nat -> Nat` is the same as `((Nat -> Nat) -> Nat)` but not `Nat -> (Nat -> Nat)`.
- Nested terms don't need brackets: `\1:Nat.\2:Nat. 2` unless enforcing application on the right. Whitespace does not matter `(\1:Nat.          1)` unless it is between application where you need at least one space.
- We consider `p z = z` as this language has no error mechanism.
- `Y` is the fabled Y combinator, use it to get general recursion. 
- To quit use `Ctrl+C` or whatever your machine uses to interrupt computations.

## Semantics

The semantics implements beta-reduction on terms and alpha-equivalence as the `Eq` instance of `PCFTerm`. The semantics are the same as the STLC but with additional rules for if, zero, pred, succ, and Y. We reformulate the semantics as [typing judgements](https://existentialtype.wordpress.com/2011/03/27/the-holy-trinity/):

for variables

<a href="https://www.codecogs.com/eqnedit.php?latex=\overline{\Gamma&space;\vdash&space;x:T},\quad&space;\mbox{(if&space;$x:T&space;\in&space;\Gamma$)}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\overline{\Gamma&space;\vdash&space;x:T},\quad&space;\mbox{(if&space;$x:T&space;\in&space;\Gamma$)}" title="\overline{\Gamma \vdash x:T},\quad \mbox{(if $x:T \in \Gamma$)}" /></a>

for abstractions:

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{\Gamma&space;,x:A&space;\vdash&space;t:B}{\Gamma&space;\vdash&space;(\lambda&space;x&space;:&space;A.&space;t)&space;:&space;B&space;}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{\Gamma&space;,x:A&space;\vdash&space;t:B}{\Gamma&space;\vdash&space;(\lambda&space;x&space;:&space;A.&space;t)&space;:&space;B&space;}" title="\frac{\Gamma ,x:A \vdash t:B}{\Gamma \vdash (\lambda x : A. t) : B }" /></a>

and application:

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{\Gamma&space;\vdash&space;f&space;:&space;A&space;\Rightarrow&space;B\quad&space;\Gamma&space;\vdash&space;x&space;:&space;A}{\Gamma&space;\vdash&space;(f&space;x)&space;:&space;B}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{\Gamma&space;\vdash&space;f&space;:&space;A&space;\Rightarrow&space;B\quad&space;\Gamma&space;\vdash&space;x&space;:&space;A}{\Gamma&space;\vdash&space;(f&space;x)&space;:&space;B}" title="\frac{\Gamma \vdash f : A \Rightarrow B\quad \Gamma \vdash x : A}{\Gamma \vdash (f x) : B}" /></a>

the reduction relation is adopted from STLC:

<a href="https://www.codecogs.com/eqnedit.php?latex=(\lambda&space;x&space;:&space;T&space;.&space;M)N&space;\rightsquigarrow&space;M&space;[x&space;:=&space;N]" target="_blank"><img src="https://latex.codecogs.com/gif.latex?(\lambda&space;x&space;:&space;T&space;.&space;M)N&space;\rightsquigarrow&space;M&space;[x&space;:=&space;N]" title="(\lambda x : T . M)N \rightsquigarrow M [x := N]" /></a>

for zero, pred, and succ:

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{&space;\Gamma&space;\vdash&space;n&space;:&space;Nat}{&space;\Gamma&space;\vdash&space;p\,&space;n&space;:&space;Nat}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{&space;\Gamma&space;\vdash&space;n&space;:&space;Nat}{&space;\Gamma&space;\vdash&space;p\,&space;n&space;:&space;Nat}" title="\frac{ \Gamma \vdash n : Nat}{ \Gamma \vdash p\, n : Nat}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\overline{\Gamma&space;\vdash&space;z:Nat}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\overline{\Gamma&space;\vdash&space;z:Nat}" title="\overline{\Gamma \vdash z:Nat}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{\Gamma&space;\vdash&space;n&space;:&space;Nat}{\Gamma&space;\vdash&space;s\,&space;n&space;:&space;Nat}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{\Gamma&space;\vdash&space;n&space;:&space;Nat}{\Gamma&space;\vdash&space;s\,&space;n&space;:&space;Nat}" title="\frac{\Gamma \vdash n : Nat}{\Gamma \vdash s\, n : Nat}" /></a>

and there are special elimination and reduction rules for general recursion:

![equation](https://latex.codecogs.com/gif.latex?\frac{\Gamma&space;\vdash&space;t&space;:&space;T&space;\rightarrow&space;T}{\Gamma&space;\vdash&space;Y\,t:T})

![equation](https://www.codecogs.com/eqnedit.php?latex=Y\,(\lambda&space;x&space;:&space;t&space;.&space;M)&space;\rightsquigarrow&space;M[x&space;:=&space;Y\,(\lambda&space;x:t.M)]" target="_blank"><img src="https://latex.codecogs.com/gif.latex?Y\,(\lambda&space;x&space;:&space;t&space;.&space;M)&space;\rightsquigarrow&space;M[x&space;:=&space;Y\,(\lambda&space;x:t.M)]" title="Y\,(\lambda x : t . M) \rightsquigarrow M[x := Y\,(\lambda x:t.M)])

or in other words:

<a href="https://www.codecogs.com/eqnedit.php?latex=Y\,&space;f&space;\rightsquigarrow\,&space;f\,&space;\(Y\,&space;f\)" target="_blank"><img src="https://latex.codecogs.com/gif.latex?Y\,&space;f&space;\rightsquigarrow\,&space;f\,&space;\(Y\,&space;f\)" title="Y\, f \rightsquigarrow\, f\, \(Y\, f\)" /></a>

- This implementation follows a [small-step](https://cs.stackexchange.com/questions/43294/difference-between-small-and-big-step-operational-semantics) operational semantics and Berendregt's [variable convention](https://cs.stackexchange.com/questions/69323/barendregts-variable-convention-what-does-it-mean) (see `substitution` in PCF.hs). 
- Reductions include the one-step reduction (see `reduce1` in PCF.hs), the many-step reduction (see `reduce` in PCF.hs). 

## Other Implementation Details
- PCF.hs contains the Haskell implementation of the calculus, including substitution, reduction, and other useful things.
- Parser.hs contains the monadic parser combinators needed to parse input strings into typed-term ASTs for the calculus.
- Repl.hs contains a simple read-eval-print loop which hooks into main, and into the parser.
- Main.hs is needed for GHC to compile without any flags, it also invokes the repl.

For contributions, see the project to-do list or submit a PR with something you think it needs.




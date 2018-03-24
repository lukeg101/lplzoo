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
- \1:Nat.1
- s s z

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

The syntax for the parser follows the non-ambiguous CFG for the System T calculus with the standard notational conventions for brackets. The full syntax is:

TODO

Some notes about the syntax:

- Variables are positive integers (including zero) as this is easy for Haskell to process, and for me implement variable generation. This is isomorphic to a whiteboard treatment using characters (like `\x:Nat.x`).
- Types are either literal `Nat` base types or nested arrow types: `T -> T`. Arrows associate to the left so that `Nat -> Nat -> Nat` is the same as `((Nat -> Nat) -> Nat)` but not `Nat -> (Nat -> Nat)`.
- Nested terms don't need brackets: `\1:Nat.\2:Nat. 2` unless enforcing application on the right. Whitespace does not matter `(\1:Nat.          1)` unless it is between application where you need at least one space.
- To quit use `Ctrl+C` or whatever your machine uses to interrupt computations.

## Semantics

The semantics implements beta-reduction on terms and alpha-equivalence as the `Eq` instance of `STTerm`. The semantics are the same as the untyped calculus with the addition of types. We reformulate the semantics as [typing judgements](https://existentialtype.wordpress.com/2011/03/27/the-holy-trinity/):

for variables:

TODO

for abstractions:

TODO

and application:

TODO

the reduction relation is adopted from STLC:

<a href="https://www.codecogs.com/eqnedit.php?latex=(\lambda&space;x&space;:&space;T&space;.&space;M)N&space;\rightsquigarrow&space;M&space;[x&space;:=&space;N]" target="_blank"><img src="https://latex.codecogs.com/gif.latex?(\lambda&space;x&space;:&space;T&space;.&space;M)N&space;\rightsquigarrow&space;M&space;[x&space;:=&space;N]" title="(\lambda x : T . M)N \rightsquigarrow M [x := N]" /></a>

and there are special reduction rules for primitive recursion on Nats:

TODO

- This implementation follows a [small-step](https://cs.stackexchange.com/questions/43294/difference-between-small-and-big-step-operational-semantics) operational semantics and Berendregt's [variable convention](https://cs.stackexchange.com/questions/69323/barendregts-variable-convention-what-does-it-mean) (see `substitution` in SystemT.hs). 
- Reductions include the one-step reduction (see `reduce1` in SystemT.hs), the many-step reduction (see `reduce` in SystemT.hs). 

## Other Implementation Details
- SystemT.hs contains the Haskell implementation of the calculus, including substitution, reduction, and other useful things.
- Parser.hs contains the monadic parser combinators needed to parse input strings into typed-term ASTs for the calculus.
- Repl.hs contains a simple read-eval-print loop which hooks into main, and into the parser.
- Main.hs is needed for GHC to compile without any flags, it also invokes the repl.

For contributions, see the [project to-do list](https://github.com/lukeg101/lplzoo/projects/3) or submit a PR with something you think it needs.

Work initially documented [here](https://gist.github.com/lukeg101/17d0a70f6ffd61978bdbe97436509571).



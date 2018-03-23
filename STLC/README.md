# Simply Typed Lambda Calculus
Haskell implementation on Alonzo Church's untyped lambda calculus. It has a single base type `O` and function type `(T->T)` to eliminate untypeable and paradoxical terms.

## Prerequisites
You need [Haskell](https://www.haskell.org/), this compiles with GHC 8.2.2 at least (Stack resolver: lts-11.0).

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
Note: When run in GHCi, you don't have the luxury of backspace, delete etc...
Compile it using GHC if you need this.

## Examples 
Where you can then have some fun, try these examples:
- `(\1:O.1)`
- `(λ1:(O->O).(λ2:O.(1 2)))`

The parser is also smart enough to recognise λ, so you can copy and paste from the output:
```
Welcome to the Simply Typed λ-calculus REPL
Type some terms or press Enter to leave.
> (\1:O.1)
(λ1:O.1)
> (λ1:O.1)
(λ1:O.1)
```

There is also a reduction tracer, which should print each reduction step. prefix any string with `'` in order to see the reductions:
```
> '(\3:O.((\1:O.1) ((\2:O.2) 3)))
(λ3:O.((λ2:O.2) 3))
(λ3:O.3)

```
Note: if you provide a non-normalizing term, the type checker will fail and reduction will not occur.

## Syntax 

The syntax follows the BNF grammar for the Simply Typed lambda calculus calculus *without* the notational conventions for brackets or combining adjacent abstractions. The full syntax is:

TODO

If you want to see the notational conventions, submit a PR! Some notes about the syntax:

- Variables are positive integers (including zero) as this is easy to for Haskell to process, and for me implement variable generation. This is isomorphic to a whiteboard treatment using characters (like `\x:O.x`).
- Nested terms require brackets: `(\1:O.(\2:O. 2))`, whitespace does not matter `(\1:O.          1)`, non-normalizing terms require you to quit with `Ctrl+C` or whatever your machine uses to interrupt computations.

## Semantics

The semantics implements beta-reduction on terms and alpha-equivalence as the `Eq` instance of `Term`:

TODO

- This implementation follows a [small-step](https://cs.stackexchange.com/questions/43294/difference-between-small-and-big-step-operational-semantics) operational semantics and Berendregt's [variable convention](https://cs.stackexchange.com/questions/69323/barendregts-variable-convention-what-does-it-mean) (see `substitution` in STLC.hs). 
- Reductions include the one-step reduction (see `reduce1` in STLC.hs), the many-step reduction (see `reduce` in STLC.hs). 

## Other Implementation Details
- STLC.hs contains the Haskell implementation of the calculus, including substitution, reduction, and other useful things.
- Parser.hs contains the monadic parser combinators needed to parse input strings into typed-term ASTs for the calculus.
- Repl.hs contains a simple read-eval-print loop which hooks into main, and into the parser.
- Main.hs is needed for GHC to compile without any flags, it also invokes the repl.

For contributions, see the [project to-do list](https://github.com/lukeg101/lplzoo/projects/2) or submit a PR with something you think it needs.

Work initially documented [here](https://gist.github.com/lukeg101/b3b305ac9438d1a57a0669f81cb0bab2).



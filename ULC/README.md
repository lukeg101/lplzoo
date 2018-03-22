# UnTyped Lambda Calculus

## Prerequisites
You need [Haskell](https://www.haskell.org/)

## To Build & Run

To compile and run do:
`ghc -O2 -o ulc Main`
then run `./ulc`

Alternatively to use the GHCi Interpreter do:
`ghci Main`
then type `main`

In either case you get something like the following:
```
Welcome to the Untyped λ-calculus REPL
Type some terms or press Enter to leave.
>

```
Note: When run in GHCi, you don't have the luxury of backspace, delete etc...
Compile it using GHC if you need this.

## Examples 
Where you can then have some fun, try these examples:
- `1`
- `(\1.1)`

The parser is also smart enough to recognise λ, so you can copy and paste from the output:
```
Welcome to the Untyped λ-calculus REPL
Type some terms or press Enter to leave.
> 1
1
> (\1.1)
(λ1.1)
> (λ1.1)
(λ1.1)
```

There is also a reduction tracer, which should print each reduction step. prefix any string with `'` in order to see the reductions:
```
> '(((\1.1) (\2.2)) 3)
((λ2.2) 3)
3
```
Note: if you provide a non-normalizing term, reductions will not terminate. Use STLC for termination guarantees.


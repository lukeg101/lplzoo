# Simply Typed Lambda Calculus

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
```haskell
Welcome to the Untyped λ-calculus REPL
Type some terms or press Enter to leave.
>

```

Where you can then have some fun, try these examples:
- `1`
- `\1.1`

The parser is also smart enough to recognise λ, so you can copy and paste from the output:
```
Welcome to the Untyped λ-calculus REPL
Type some terms or press Enter to leave.
> \1.1
(λ1.1)
> (λ1.1)
(λ1.1)
>
```



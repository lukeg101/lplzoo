# UnTyped Lambda Calculus

## Prerequisites
You need [Haskell](https://www.haskell.org/), this compiles with GHC 8.2.2 at least (Stack resolver: lts-11.0).

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

## Syntax 

The syntax follows the BNF grammar for the untyped calculus *without* the notational conventions for brackets or combining adjacent abstractions. The full syntax is:

<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}&space;&&\\&space;\mathbf{\tau}&&space;::=&space;&&space;\mathbf{\upsilon}\\&space;&&space;|&space;&&space;(\mathbf{\tau&space;\tau})&space;\\&space;&&space;|&space;&&space;(\lambda&space;\mathbf{\upsilon}&space;.&space;\mathbf{\tau})\\&space;&&\\&space;\upsilon&space;&&space;::=&space;&&space;0&space;|&space;1&space;|&space;2&space;|&space;...&space;\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}&space;&&\\&space;\mathbf{\tau}&&space;::=&space;&&space;\mathbf{\upsilon}\\&space;&&space;|&space;&&space;(\mathbf{\tau&space;\tau})&space;\\&space;&&space;|&space;&&space;(\lambda&space;\mathbf{\upsilon}&space;.&space;\mathbf{\tau})\\&space;&&\\&space;\upsilon&space;&&space;::=&space;&&space;0&space;|&space;1&space;|&space;2&space;|&space;...&space;\end{matrix}" title="\begin{matrix} &&\\ \mathbf{\tau}& ::= & \mathbf{\upsilon}\\ & | & (\mathbf{\tau \tau}) \\ & | & (\lambda \mathbf{\upsilon} . \mathbf{\tau})\\ &&\\ \upsilon & ::= & 0 | 1 | 2 | ... \end{matrix}" /></a>

If you want to see the notational conventions, submit a PR! Some notes about the syntax:

- Variables are positive integers (including zero) as this is easy to for Haskell to process, and for me implement variable generation. This is isomorphic to a whiteboard treatment using characters (like `\x.x`).
- Nested terms require brackets: `(\1.(1 1))`, whitespace does not matter `(\1      .(1 1))`, non-terminating terms require you to quit with `Ctrl+C` or whatever your machine uses to interupt computations. 



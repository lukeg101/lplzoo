# Untyped Lambda Calculus
Haskell implementation on Alonzo Church's untyped lambda calculus. It's a Turing Complete model of computation.

This calculus is considered the canonical _hello world_ of functional programming languages.

## Prerequisites
You need [Haskell](https://www.haskell.org/), this compiles with GHC 8.2.2 at least (Stack resolver: lts-11.0).

_Optional_: If you want to run the tests for this module, you'll need [QuickCheck](http://hackage.haskell.org/package/QuickCheck-2.12.6.1/docs/Test-QuickCheck.html#v:label).

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
Note: When run in GHCi, you don't have the luxury of escaped characters, backspace, delete etc...
Compile it using GHC if you need this.

## Examples
Where you can then have some fun, try these examples:
- `x`
- `\x.x`

The parser is also smart enough to recognise λ, so you can copy and paste from the output:
```
Welcome to the Untyped λ-calculus REPL
Type some terms or press Enter to leave.
>   x
=   x
>   \x.x
=   λx.x
>   λx.x
=   λx.x
```
`>` denotes the REPL waiting for input, `=` means no reductions occurred (it's the same term), `~>` denotes one reduction, and `~>*` denotes 0 or more reductions (although in practice this is 1 or more due to `=`).

There is also a reduction tracer, which should print each reduction step. prefix any string with `'` in order to see the reductions:
```
>   '(\x.x) (\y.y) z
~>  (λy.y) z
~>  z
```
Note: if you provide a non-normalizing term, reductions will not terminate. Use STLC for termination guarantees.

You can save variables for the life of the program with a `let` expression. Any time a saved variable appears in a term, it will be substituted for the saved term:
```
>   let x = y
Saved: y
>   x
=   y
>   \y.x
=   λz.y
```
Note: Consequently `let` and `=` are keywords, and so you cannot name variables with these.

Note: We do capture avoiding substitution if a bound variable is the same as one we are substituting in. We rename the bound term to a new variable.

## Syntax

We base the language on the BNF for the untyped calculus:

<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}&space;\mathbf{\tau}&&space;::=&space;&&space;\lambda&space;\mathbf{\upsilon}&space;.&space;\mathbf{\tau}\\&space;&&space;|&space;&&space;\tau\,{\tt&space;space}\,\tau\\&space;&&space;|&space;&&space;\upsilon&space;&&\\&space;\upsilon&space;&&space;::=&space;&&space;\tt{a}&space;|&space;\tt{b}&space;|&space;\tt{c}&space;|&space;...&space;|&space;\tt{aa}&space;|&space;...&space;\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}&space;\mathbf{\tau}&&space;::=&space;&&space;\lambda&space;\mathbf{\upsilon}&space;.&space;\mathbf{\tau}\\&space;&&space;|&space;&&space;\tau\,{\tt&space;space}\,\tau\\&space;&&space;|&space;&&space;\upsilon&space;&&\\&space;\upsilon&space;&&space;::=&space;&&space;\tt{a}&space;|&space;\tt{b}&space;|&space;\tt{c}&space;|&space;...&space;|&space;\tt{aa}&space;|&space;...&space;\end{matrix}" title="\begin{matrix} \mathbf{\tau}& ::= & \lambda \mathbf{\upsilon} . \mathbf{\tau}\\ & | & \tau\,{\tt space}\,\tau\\ & | & \upsilon &&\\ \upsilon & ::= & \tt{a} | \tt{b} | \tt{c} | ... | \tt{aa} | ... \end{matrix}" /></a>

However we adopt the standard bracketing conventions to eliminate ambiguity in the parser. Concretely, the parser implements the non-ambiguous grammar as follows:

<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}&space;&&\\&space;\mathbf{\tau}&&space;::=&space;&&space;\lambda&space;\mathbf{\upsilon}&space;.&space;\mathbf{\tau}\\&space;&&space;|&space;&&space;\alpha\\&space;&&\\&space;\alpha&space;&&space;::=&space;&&space;\beta&space;\\&space;&|&space;&\mathbf{\alpha\,&space;\tt{space}\,&space;\beta}&space;\\&space;&&\\&space;\beta&space;&&space;::=&space;&&space;\tt{(}\tau&space;\tt{)}\\&space;&|&&space;\upsilon&space;\\&space;&&\\&space;\upsilon&space;&&space;::=&space;&&space;\tt{a}&space;|&space;\tt{b}&space;|&space;\tt{c}&space;|&space;...&space;|&space;\tt{aa}&space;|&space;...&space;\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}&space;&&\\&space;\mathbf{\tau}&&space;::=&space;&&space;\lambda&space;\mathbf{\upsilon}&space;.&space;\mathbf{\tau}\\&space;&&space;|&space;&&space;\alpha\\&space;&&\\&space;\alpha&space;&&space;::=&space;&&space;\beta&space;\\&space;&|&space;&\mathbf{\alpha\,&space;\tt{space}\,&space;\beta}&space;\\&space;&&\\&space;\beta&space;&&space;::=&space;&&space;\tt{(}\tau&space;\tt{)}\\&space;&|&&space;\upsilon&space;\\&space;&&\\&space;\upsilon&space;&&space;::=&space;&&space;\tt{a}&space;|&space;\tt{b}&space;|&space;\tt{c}&space;|&space;...&space;|&space;\tt{aa}&space;|&space;...&space;\end{matrix}" title="\begin{matrix} &&\\ \mathbf{\tau}& ::= & \lambda \mathbf{\upsilon} . \mathbf{\tau}\\ & | & \alpha\\ &&\\ \alpha & ::= & \beta \\ &| &\mathbf{\alpha\, \tt{space}\, \beta} \\ &&\\ \beta & ::= & \tt{(}\tau \tt{)}\\ &|& \upsilon \\ &&\\ \upsilon & ::= & \tt{a} | \tt{b} | \tt{c} | ... | \tt{aa} | ... \end{matrix}" /></a>

Some notes about the syntax:

- The above syntax only covers the core calculus, and not the repl extensions (such as `let` bindings above). The extensions are simply added on in the repl.
- Variables are strings (excluding numbers), as this is isomorphic to a whiteboard treatment and hence the most familiar.
- Nested terms may not require brackets: `\x.x x` and follows the convention of abstractions being left associative, application being right associative, and application having higher precedence than abstraction.
- Whitespace does not matter `\x.(x    x)`, except in between application where a minimum of one space is needed.
- Non-terminating terms require you to quit with `Ctrl+C` or whatever your machine uses to interrupt computations.
- This grammar left-recursive and non-ambiguous.

## Semantics

The semantics implements beta-reduction on terms and alpha-equivalence as the `Eq` instance of `Term`:

<a href="https://www.codecogs.com/eqnedit.php?latex=(\lambda&space;x&space;.&space;M)N&space;\rightsquigarrow&space;M&space;[x&space;:=&space;N]" target="_blank"><img src="https://latex.codecogs.com/gif.latex?(\lambda&space;x&space;.&space;M)N&space;\rightsquigarrow&space;M&space;[x&space;:=&space;N]" title="(\lambda x . M)N \rightsquigarrow M [x := N]" /></a>

- This implementation follows a [small-step](https://cs.stackexchange.com/questions/43294/difference-between-small-and-big-step-operational-semantics) operational semantics and Berendregt's [variable convention](https://cs.stackexchange.com/questions/69323/barendregts-variable-convention-what-does-it-mean) (see `substitution` in ULC.hs).
- Reductions include the one-step reduction (see `reduce1` in ULC.hs), the many-step reduction (see `reduce` in ULC.hs).

## Other Implementation Details
- ULC.hs contains the Haskell implementation of the calculus, including substitution, reduction, and other useful things.
- Parser.hs contains the monadic parser combinators needed to parse input strings into term ASTs for the calculus. It follows the grammar above.
- Repl.hs contains a simple read-eval-print loop which hooks into main, and into the parser. It uses [Haskeline](https://hackage.haskell.org/package/haskeline) to provide a line-editing interface, input history, and tab-completion of named terms saved in the environment.
- Main.hs is needed for GHC to compile without any flags, it also invokes the repl.
- Tests.hs is the test suite. We run have unit tests for terms in the language. QuickCheck is used to generate arbitrary trees and test they are parsed and printed correctly.

For contributions, see the [project to-do list](https://github.com/lukeg101/lplzoo/projects/1), [Contributions](https://github.com/lukeg101/lplzoo#contributions), and submit a PR with something you think it needs.

Work initially documented [here](https://gist.github.com/lukeg101/9090f20f4a7b09f401df9390a0e357c9).

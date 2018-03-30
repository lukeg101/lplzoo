# Lambda Mu Calculus
Haskell implementation on Michel Parigot's λμ calculus. It has the features of STLC, but you can name types (not system F!) and use `μ` and `[_]` operators.

This strongly normalising calculus encodes [Classical Natural Deduction](https://www.cs.ru.nl/~freek/courses/tt-2011/papers/parigot.pdf) in a programming language. [Hofmann and Streicher](https://pdfs.semanticscholar.org/24ec/2e8104e20983cd747ab6868265559ab7db01.pdf) later discovered it captures [continuation semantics](https://en.wikipedia.org/wiki/Continuation-passing_style) for programming languages.

## Prerequisites
You need [Haskell](https://www.haskell.org/), this compiles with GHC 8.2.2 at least (Stack resolver: lts-11.0).

## To Build & Run

To compile and run do:
`ghc -O2 -o mu Main`
then run `./mu`

Alternatively to use the GHCi Interpreter do:
`ghci Main`
then type `main`

In either case you get something like the following:
```
Welcome to the λμ-calculus REPL
Type some terms or press Enter to leave.
>
```
Note: When run in GHCi, you don't have the luxury of escaped characters, backspace, delete etc...
Compile it using GHC if you need this.

## Examples 
Where you can then have some fun, try these examples:
- `\1:A.1`
- `λ0:(A->B)->A.μ1:A.[1]0 (λ2:A.μ3:B.[1]2)` an encoding of [_Peirce's Law_](https://en.wikipedia.org/wiki/Peirce%27s_law).

Note: `λ` is the lambda abstraction, `μ` is the mu(control) abstraction, `[2]` is the bracketing operator for μ-variable `2`. Alternatively typed as `\`, `M`, and `[2]` respectively.

The parser is also smart enough to recognise λ, so you can copy and paste from the output:
```
Welcome to the λμ-calculus REPL
Type some terms or press Enter to leave.
> \1:A.1
λ1:A.1
> λ1:A.1
λ1:A.1
```

There is also a reduction tracer, which should print each reduction step. prefix any string with `'` in order to see the reductions:
```
TODO
```

There is also a typing mechanism, which should display the type or fail as usual.
```
> tλ0:(A->B)->A.μ1:A.[1]0 (λ2:A.μ3:B.[1]2)
((A->B)->A)->A
> \1:A.1 1
Cannot Type Term: \1:A.1 1
```

Note: if you provide a non-normalizing term, the type checker will fail and reduction will not occur.

## Syntax 

We base the language on the BNF for the typed calculus:

<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}&space;\mathbf{\tau}&&space;::=&space;&&space;\lambda&space;\mathbf{\upsilon}\tt{:}\sigma&space;.&space;\mathbf{\tau}\\&space;&&space;|&space;&&space;\tau\,&space;{\tt&space;space}\,&space;\tau\\&space;&&space;|&space;&&space;\upsilon&space;\\&space;&|&&space;\mu\upsilon:\sigma.&space;\[&space;\upsilon&space;\]\tau&space;&&\\&space;&&\\&space;\upsilon&space;&&space;::=&space;&&space;\tt{0}&space;|&space;\tt{1}&space;|&space;\tt{2}&space;|&space;...&space;\\&space;&&\\&space;\sigma&space;&&space;::=&space;&&space;{\tt&space;X,Y,Z}...\\&space;&&space;|&space;&&space;\sigma&space;\rightarrow&space;\sigma&space;\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}&space;\mathbf{\tau}&&space;::=&space;&&space;\lambda&space;\mathbf{\upsilon}\tt{:}\sigma&space;.&space;\mathbf{\tau}\\&space;&&space;|&space;&&space;\tau\,&space;{\tt&space;space}\,&space;\tau\\&space;&&space;|&space;&&space;\upsilon&space;\\&space;&|&&space;\mu\upsilon:\sigma.&space;\[&space;\upsilon&space;\]\tau&space;&&\\&space;&&\\&space;\upsilon&space;&&space;::=&space;&&space;\tt{0}&space;|&space;\tt{1}&space;|&space;\tt{2}&space;|&space;...&space;\\&space;&&\\&space;\sigma&space;&&space;::=&space;&&space;{\tt&space;X,Y,Z}...\\&space;&&space;|&space;&&space;\sigma&space;\rightarrow&space;\sigma&space;\end{matrix}" title="\begin{matrix} \mathbf{\tau}& ::= & \lambda \mathbf{\upsilon}\tt{:}\sigma . \mathbf{\tau}\\ & | & \tau\, {\tt space}\, \tau\\ & | & \upsilon \\ &|& \mu\upsilon:\sigma. \[ \upsilon \]\tau &&\\ &&\\ \upsilon & ::= & \tt{0} | \tt{1} | \tt{2} | ... \\ &&\\ \sigma & ::= & {\tt X,Y,Z}...\\ & | & \sigma \rightarrow \sigma \end{matrix}" /></a>


However we adopt standard bracketing conventions to eliminate ambiguity in the parser. Concretely, the parser implements the non-ambiguous grammar as follows:

TODO

Some notes about the syntax:

- Variables are positive integers (including zero) as this is easy for Haskell to process, and for me implement variable generation. This is isomorphic to a whiteboard treatment using characters (like `\x:A.x`).
- Types are either chars `A,B,C,...` base types or nested arrow types: `T -> T`. Arrows associate to the right so that `T -> T -> T` is the same as `T -> (T -> T)` but not `((T -> T) -> T)`. This differs from the approach in STLC, where the the emphasis is on base `O` versus arrow types `(O->O)->O` etc... 
- Type equality is strict syntactic equality, so A = A but not A = B (think of them as propositions!). Term equality adopts alpha-equivalence (assuming types match)
- Nested terms don't need brackets: `\1:A.\2:B. 2` unless enforcing application on the right. Whitespace does not matter `(\1:A.          1)` unless it is between application where you need at least one space.
- To quit use `Ctrl+C` or whatever your machine uses to interrupt computations.

## Semantics

The semantics implements beta-reduction on terms and alpha-equivalence as the `Eq` instance of `MuTerm`. The semantics are the same as STLC with the addition of rules for μ and [] operators. We reformulate the semantics as [typing judgements](https://existentialtype.wordpress.com/2011/03/27/the-holy-trinity/):

for variables (adopted from STLC):

<a href="https://www.codecogs.com/eqnedit.php?latex=\overline{\Gamma&space;\vdash&space;x:T},\quad&space;\mbox{(if&space;$x:T&space;\in&space;\Gamma$)}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\overline{\Gamma&space;\vdash&space;x:T},\quad&space;\mbox{(if&space;$x:T&space;\in&space;\Gamma$)}" title="\overline{\Gamma \vdash x:T},\quad \mbox{(if $x:T \in \Gamma$)}" /></a>

for abstractions (adopted from STLC):

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{\Gamma&space;,x:A&space;\vdash&space;t:B}{\Gamma&space;\vdash&space;(\lambda&space;x&space;:&space;A.&space;t)&space;:&space;B&space;}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{\Gamma&space;,x:A&space;\vdash&space;t:B}{\Gamma&space;\vdash&space;(\lambda&space;x&space;:&space;A.&space;t)&space;:&space;B&space;}" title="\frac{\Gamma ,x:A \vdash t:B}{\Gamma \vdash (\lambda x : A. t) : B }" /></a>

and application (adopted from STLC):

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{\Gamma&space;\vdash&space;f&space;:&space;A&space;\Rightarrow&space;B\quad&space;\Gamma&space;\vdash&space;x&space;:&space;A}{\Gamma&space;\vdash&space;(f&space;x)&space;:&space;B}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{\Gamma&space;\vdash&space;f&space;:&space;A&space;\Rightarrow&space;B\quad&space;\Gamma&space;\vdash&space;x&space;:&space;A}{\Gamma&space;\vdash&space;(f&space;x)&space;:&space;B}" title="\frac{\Gamma \vdash f : A \Rightarrow B\quad \Gamma \vdash x : A}{\Gamma \vdash (f x) : B}" /></a>

the reduction relation (adopted from STLC):

<a href="https://www.codecogs.com/eqnedit.php?latex=(\lambda&space;x&space;:&space;T&space;.&space;M)N&space;\rightsquigarrow&space;M&space;[x&space;:=&space;N]" target="_blank"><img src="https://latex.codecogs.com/gif.latex?(\lambda&space;x&space;:&space;T&space;.&space;M)N&space;\rightsquigarrow&space;M&space;[x&space;:=&space;N]" title="(\lambda x : T . M)N \rightsquigarrow M [x := N]" /></a>

and special introduction, elimination, and reduction rules for μ and [] operators:

TODO

- This implementation follows a [small-step](https://cs.stackexchange.com/questions/43294/difference-between-small-and-big-step-operational-semantics) operational semantics and Berendregt's [variable convention](https://cs.stackexchange.com/questions/69323/barendregts-variable-convention-what-does-it-mean) (see `substitution` in Mu.hs). 
- Reductions include the one-step reduction (see `reduce1` in Mu.hs), the many-step reduction (see `reduce` in Mu.hs). 

## Other Implementation Details
- Mu.hs contains the Haskell implementation of the calculus, including substitution, reduction, and other useful things.
- Parser.hs contains the monadic parser combinators needed to parse input strings into typed-term ASTs for the calculus.
- Repl.hs contains a simple read-eval-print loop which hooks into main, and into the parser.
- Main.hs is needed for GHC to compile without any flags, it also invokes the repl.

For contributions, see the project to-do list or submit a PR with something you think it needs.




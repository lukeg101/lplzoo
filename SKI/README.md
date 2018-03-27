# SKI Combinator Calculus
Haskell implementation on  Moses Schonfinkel's SKI-combintator calculus. It's a Turing Complete model of computation consisting purely of applying terms `S`, `K` and `I`.

This calculus can be used as a basis for all combinatory logic.

## Prerequisites
You need [Haskell](https://www.haskell.org/), this compiles with GHC 8.2.2 at least (Stack resolver: lts-11.0).

## To Build & Run

To compile and run do:
`ghc -O2 -o ski Main`
then run `./ski`

Alternatively to use the GHCi Interpreter do:
`ghci Main`
then type `main`

In either case you get something like the following:
```
Welcome to the SKI combinator calculus REPL
Type some terms or press Enter to leave.
>
```

Note: When run in GHCi, you don't have the luxury of escaped characters, backspace, delete etc...
Compile it using GHC if you need this.

## Examples 
Where you can then have some fun, try these examples:
- `S K K 1`
- `S (K S) K` (known as B)
- `S (S (K (S (K S) K)) S) (K K)` (known as C)

You can copy and paste from the output:
```
Welcome to the SKI combinator calculus REPL
Type some terms or press Enter to leave.
> S K K 1
1
> 1
1
```

There is also a reduction tracer, which should print each reduction step. prefix any string with `'` in order to see the reductions:
```
> 'S (S (K S) (S (K K) I)) (S (S (K S) (S (K K) I)) (K I)) (S (S (K S) (S (K K) I)) (S (S (K S) (S (K K) I)) (K I)))
S (K S) (S (K K) I) (S (S (K S) (S (K K) I)) (S (S (K S) (S (K K) I)) (K I))) (S (S (K S) (S (K K) I)) (K I) (S (S (K S) (S (K K) I)) (S (S (K S) (S (K K) I)) (K I))))
K S (S (S (K S) (S (K K) I)) (S (S (K S) (S (K K) I)) (K I))) (S (K K) I (S (S (K S) (S (K K) I)) (S (S (K S) (S (K K) I)) (K I)))) (S (S (K S) (S (K K) I)) (K I) (S (S (K S) (S (K K) I)) (S (S (K S) (S (K K) I)) (K I))))
S (S (K K) I (S (S (K S) (S (K K) I)) (S (S (K S) (S (K K) I)) (K I)))) (S (S (K S) (S (K K) I)) (K I) (S (S (K S) (S (K K) I)) (S (S (K S) (S (K K) I)) (K I))))
S (K K (S (S (K S) (S (K K) I)) (S (S (K S) (S (K K) I)) (K I))) (I (S (S (K S) (S (K K) I)) (S (S (K S) (S (K K) I)) (K I))))) (S (S (K S) (S (K K) I)) (K I) (S (S (K S) (S (K K) I)) (S (S (K S) (S (K K) I)) (K I))))
S (K (I (S (S (K S) (S (K K) I)) (S (S (K S) (S (K K) I)) (K I))))) (S (S (K S) (S (K K) I)) (K I) (S (S (K S) (S (K K) I)) (S (S (K S) (S (K K) I)) (K I))))
S (K (S (S (K S) (S (K K) I)) (S (S (K S) (S (K K) I)) (K I)))) (S (S (K S) (S (K K) I)) (K I) (S (S (K S) (S (K K) I)) (S (S (K S) (S (K K) I)) (K I))))
S (K (S (S (K S) (S (K K) I)) (S (S (K S) (S (K K) I)) (K I)))) (S (K S) (S (K K) I) (S (S (K S) (S (K K) I)) (S (S (K S) (S (K K) I)) (K I))) (K I (S (S (K S) (S (K K) I)) (S (S (K S) (S (K K) I)) (K I)))))
S (K (S (S (K S) (S (K K) I)) (S (S (K S) (S (K K) I)) (K I)))) (K S (S (S (K S) (S (K K) I)) (S (S (K S) (S (K K) I)) (K I))) (S (K K) I (S (S (K S) (S (K K) I)) (S (S (K S) (S (K K) I)) (K I)))) (K I (S (S (K S) (S (K K) I)) (S (S (K S) (S (K K) I)) (K I)))))
S (K (S (S (K S) (S (K K) I)) (S (S (K S) (S (K K) I)) (K I)))) (S (S (K K) I (S (S (K S) (S (K K) I)) (S (S (K S) (S (K K) I)) (K I)))) (K I (S (S (K S) (S (K K) I)) (S (S (K S) (S (K K) I)) (K I)))))
S (K (S (S (K S) (S (K K) I)) (S (S (K S) (S (K K) I)) (K I)))) (S (K K (S (S (K S) (S (K K) I)) (S (S (K S) (S (K K) I)) (K I))) (I (S (S (K S) (S (K K) I)) (S (S (K S) (S (K K) I)) (K I))))) (K I (S (S (K S) (S (K K) I)) (S (S (K S) (S (K K) I)) (K I)))))
S (K (S (S (K S) (S (K K) I)) (S (S (K S) (S (K K) I)) (K I)))) (S (K (I (S (S (K S) (S (K K) I)) (S (S (K S) (S (K K) I)) (K I))))) (K I (S (S (K S) (S (K K) I)) (S (S (K S) (S (K K) I)) (K I)))))
S (K (S (S (K S) (S (K K) I)) (S (S (K S) (S (K K) I)) (K I)))) (S (K (S (S (K S) (S (K K) I)) (S (S (K S) (S (K K) I)) (K I)))) (K I (S (S (K S) (S (K K) I)) (S (S (K S) (S (K K) I)) (K I)))))
S (K (S (S (K S) (S (K K) I)) (S (S (K S) (S (K K) I)) (K I)))) (S (K (S (S (K S) (S (K K) I)) (S (S (K S) (S (K K) I)) (K I)))) I)
```
Note: the above computes 2^2 in Church Numeral format

Note: if you provide a non-normalizing term (e.g. with `(S I I (S I I))` which is the same as `(\1.1 1)(\1.1 1)` in ULC), reductions will not terminate. Use STLC for termination guarantees.

## Syntax 

We base the language on the BNF for the SKI calculus:

<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}&space;\mathbf{\tau}&&space;::=&space;&&space;\mathbf{\upsilon}\\&space;&&space;|&space;&&space;\tau\,{\tt&space;space}\,\tau\\&space;&&space;|&space;&&space;S\,\tau\,\tau\,\tau&space;&&\\&space;&&space;|&space;&&space;K\,&space;\tau\,\tau&space;&&\\&space;&&space;|&space;&&space;I\,&space;\tau&&\\&space;\upsilon&space;&&space;::=&space;&&space;\tt{0}&space;|&space;\tt{1}&space;|&space;\tt{2}&space;|&space;...&space;\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}&space;\mathbf{\tau}&&space;::=&space;&&space;\mathbf{\upsilon}\\&space;&&space;|&space;&&space;\tau\,{\tt&space;space}\,\tau\\&space;&&space;|&space;&&space;S\,\tau\,\tau\,\tau&space;&&\\&space;&&space;|&space;&&space;K\,&space;\tau\,\tau&space;&&\\&space;&&space;|&space;&&space;I\,&space;\tau&&\\&space;\upsilon&space;&&space;::=&space;&&space;\tt{0}&space;|&space;\tt{1}&space;|&space;\tt{2}&space;|&space;...&space;\end{matrix}" title="\begin{matrix} \mathbf{\tau}& ::= & \mathbf{\upsilon}\\ & | & \tau\,{\tt space}\,\tau\\ & | & S\,\tau\,\tau\,\tau &&\\ & | & K\, \tau\,\tau &&\\ & | & I\, \tau&&\\ \upsilon & ::= & \tt{0} | \tt{1} | \tt{2} | ... \end{matrix}" /></a>

However we adopt the standard bracketing conventions to eliminate ambiguity in the parser. Concretely, the parser implements the non-ambiguous grammar as follows:

<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}&space;\tau&space;&&space;::=&space;&&space;\beta&space;\\&space;&|&space;&\mathbf{\tau\,&space;\tt{space}\,&space;\beta}&space;\\&space;&&\\&space;\beta&space;&&space;::=&space;&&space;\tt{(}\tau&space;\tt{)}\\&space;&|&&space;\upsilon&space;\\&space;&|&\tt{S}\\&space;&|&\tt{K}\\&space;&|&\tt{I}\\&space;&&\\&space;\upsilon&::=&&space;{\tt&space;0}&space;|&space;{\tt&space;1}&space;|&space;{\tt&space;2}&space;|&space;...&space;\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}&space;\tau&space;&&space;::=&space;&&space;\beta&space;\\&space;&|&space;&\mathbf{\tau\,&space;\tt{space}\,&space;\beta}&space;\\&space;&&\\&space;\beta&space;&&space;::=&space;&&space;\tt{(}\tau&space;\tt{)}\\&space;&|&&space;\upsilon&space;\\&space;&|&\tt{S}\\&space;&|&\tt{K}\\&space;&|&\tt{I}\\&space;&&\\&space;\upsilon&::=&&space;{\tt&space;0}&space;|&space;{\tt&space;1}&space;|&space;{\tt&space;2}&space;|&space;...&space;\end{matrix}" title="\begin{matrix} \tau & ::= & \beta \\ &| &\mathbf{\tau\, \tt{space}\, \beta} \\ &&\\ \beta & ::= & \tt{(}\tau \tt{)}\\ &|& \upsilon \\ &|&\tt{S}\\ &|&\tt{K}\\ &|&\tt{I}\\ &&\\ \upsilon&::=& {\tt 0} | {\tt 1} | {\tt 2} | ... \end{matrix}" /></a>

Some notes about the syntax:

- Variables are positive integers (including zero) as this is easy to for Haskell to process, and for me to implement. This is isomorphic to a whiteboard treatment using characters. Some treatments of the SKI calculus omit variables, but as long as there is no notion of binding it should be ok.
- Nested terms may not require brackets and follows the convention of application being left associative where `S K K 1` is the same as `((S K) K) 1`, but not `S (K (K 1))`.
- Whitespace does not matter, except in between application where a minimumum of one space is needed. 
- Non-terminating terms require you to quit with `Ctrl+C` or whatever your machine uses to interupt computations.
- This grammar left-recursive and non-ambiguous.

## Semantics

The SKI calculus operates on combinations of `S`, `K`, and `I`, which can be formed with introduction and elimination rules:

<a href="https://www.codecogs.com/eqnedit.php?latex=\overline{x&space;\in&space;CL},&space;\mbox{(if&space;$x&space;\in&space;\upsilon$)}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\overline{x&space;\in&space;CL},&space;\mbox{(if&space;$x&space;\in&space;\upsilon$)}" title="\overline{x \in CL}, \mbox{(if $x \in \upsilon$)}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\overline{I&space;\in&space;CL}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\overline{I&space;\in&space;CL}" title="\overline{I \in CL}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\overline{S&space;\in&space;CL}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\overline{S&space;\in&space;CL}" title="\overline{S \in CL}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\overline{K&space;\in&space;CL}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\overline{K&space;\in&space;CL}" title="\overline{K \in CL}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{x&space;\in&space;CL\quad&space;y&space;\in&space;CL}{(x\,y)&space;\in&space;CL}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{x&space;\in&space;CL\quad&space;y&space;\in&space;CL}{(x\,y)&space;\in&space;CL}" title="\frac{x \in CL\quad y \in CL}{(x\,y) \in CL}" /></a>

and reduced with rules:

<a href="https://www.codecogs.com/eqnedit.php?latex=I\,x&space;\rightsquigarrow&space;x\newline&space;K\,x\,y&space;\rightsquigarrow&space;x\newline&space;S\,x\,y\,z&space;\rightsquigarrow&space;x\,z\,(y\,z)" target="_blank"><img src="https://latex.codecogs.com/gif.latex?I\,x&space;\rightsquigarrow&space;x\newline&space;K\,x\,y&space;\rightsquigarrow&space;x\newline&space;S\,x\,y\,z&space;\rightsquigarrow&space;x\,z\,(y\,z)" title="I\,x \rightsquigarrow x\newline K\,x\,y \rightsquigarrow x\newline S\,x\,y\,z \rightsquigarrow x\,z\,(y\,z)" /></a>

- This implementation follows a [small-step](https://cs.stackexchange.com/questions/43294/difference-between-small-and-big-step-operational-semantics) operational semantics and has free variables for simplicity. 
- Reductions include the one-step reduction (see `reduce1` in SKI.hs), the many-step reduction (see `reduce` in SKI.hs). 

## Other Implementation Details
- SKI.hs contains the Haskell implementation of the calculus, including substitution, reduction, and other useful things.
- Parser.hs contains the monadic parser combinators needed to parse input strings into term ASTs for the calculus. It follows the grammar above.
- Repl.hs contains a simple read-eval-print loop which hooks into main, and into the parser.
- Main.hs is needed for GHC to compile without any flags, it also invokes the repl.

For contributions, see the project to-do list or submit a PR with something you think it needs.



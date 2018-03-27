# Programming Computable Functions
Haskell implementation on Gordon Plotkin's typed lambda calculus. It has a base type `Nat`, the function type `T->T`, `Succ` \\ `Pred`essors on Nats, and [general](https://stackoverflow.com/questions/1712237/how-does-primitive-recursion-differ-from-normal-recursion) recursion on Nats through use of a [Y combinator](https://en.wikipedia.org/wiki/Fixed-point_combinator#Fixed_point_combinators_in_lambda_calculus). It is Turing Complete, but not strongly normalizing for all terms.

PCF is considered to be a simplified version of modern functional languages such as Haskell; where non-nonsensical terms are prohibited by the type system but recursion is afforded by `Y`. 

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
- `Y (λ0:Nat->Nat.λ1:Nat.if 1 z (if (p 1) (s z) (0 (p (p 1))))) (s (s z))`

The parser is also smart enough to recognise λ, so you can copy and paste from the output:
```
> \1:Nat.1
λ1:Nat.1
> λ1:Nat.1
λ1:Nat.1
```

There is also a reduction tracer, which should print each reduction step. prefix any string with `'` in order to see the reductions:
```
> 'Y (λ0:Nat->Nat.λ1:Nat.if 1 z (if (p 1) (s z) (0 (p (p 1))))) (s (s z))
(λ1:Nat.if 1 z (if (p 1) (s z) (Y (λ0:Nat->Nat.λ1:Nat.if 1 z (if (p 1) (s z) (0 (p (p 1))))) (p (p 1))))) (s (s z))
if (s (s z)) z (if (p (s (s z))) (s z) (Y (λ0:Nat->Nat.λ1:Nat.if 1 z (if (p 1) (s z) (0 (p (p 1))))) (p (p (s (s z))))))
if (p (s (s z))) (s z) (Y (λ0:Nat->Nat.λ1:Nat.if 1 z (if (p 1) (s z) (0 (p (p 1))))) (p (p (s (s z)))))
if (s z) (s z) (Y (λ0:Nat->Nat.λ1:Nat.if 1 z (if (p 1) (s z) (0 (p (p 1))))) (p (p (s (s z)))))
Y (λ0:Nat->Nat.λ1:Nat.if 1 z (if (p 1) (s z) (0 (p (p 1))))) (p (p (s (s z))))
(λ1:Nat.if 1 z (if (p 1) (s z) (Y (λ0:Nat->Nat.λ1:Nat.if 1 z (if (p 1) (s z) (0 (p (p 1))))) (p (p 1))))) (p (p (s (s z))))
if (p (p (s (s z)))) z (if (p (p (p (s (s z))))) (s z) (Y (λ0:Nat->Nat.λ1:Nat.if 1 z (if (p 1) (s z) (0 (p (p 1))))) (p (p (p (p (s (s z))))))))
if (p (s z)) z (if (p (p (p (s (s z))))) (s z) (Y (λ0:Nat->Nat.λ1:Nat.if 1 z (if (p 1) (s z) (0 (p (p 1))))) (p (p (p (p (s (s z))))))))
if z z (if (p (p (p (s (s z))))) (s z) (Y (λ0:Nat->Nat.λ1:Nat.if 1 z (if (p 1) (s z) (0 (p (p 1))))) (p (p (p (p (s (s z))))))))
z
```
Note: the above is a function to check if 2 is even.

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

We base the language on the BNF for PCF:

<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}&space;\mathbf{\tau}&&space;::=&space;&&space;\lambda&space;\mathbf{\upsilon}{\tt&space;:}\sigma&space;.&space;\mathbf{\tau}\\&space;&&space;|&space;&&space;\tau\,&space;\tau\\&space;&&space;|&space;&&space;\upsilon&space;\\&space;&|&&space;p\,\tau&space;\\&space;&|&&space;s\,\tau\\&space;&|&&space;if\,\tau\,\tau\,\tau\\&space;&|&&space;z&space;\\&space;&|&&space;Y\,\tau\\&space;&&\\&space;\upsilon&space;&&space;::=&space;&&space;\tt{0}&space;|&space;\tt{1}&space;|&space;\tt{2}&space;|&space;...&space;\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}&space;\mathbf{\tau}&&space;::=&space;&&space;\lambda&space;\mathbf{\upsilon}{\tt&space;:}\sigma&space;.&space;\mathbf{\tau}\\&space;&&space;|&space;&&space;\tau\,&space;\tau\\&space;&&space;|&space;&&space;\upsilon&space;\\&space;&|&&space;p\,\tau&space;\\&space;&|&&space;s\,\tau\\&space;&|&&space;if\,\tau\,\tau\,\tau\\&space;&|&&space;z&space;\\&space;&|&&space;Y\,\tau\\&space;&&\\&space;\upsilon&space;&&space;::=&space;&&space;\tt{0}&space;|&space;\tt{1}&space;|&space;\tt{2}&space;|&space;...&space;\end{matrix}" title="\begin{matrix} \mathbf{\tau}& ::= & \lambda \mathbf{\upsilon}{\tt :}\sigma . \mathbf{\tau}\\ & | & \tau\, \tau\\ & | & \upsilon \\ &|& p\,\tau \\ &|& s\,\tau\\ &|& if\,\tau\,\tau\,\tau\\ &|& z \\ &|& Y\,\tau\\ &&\\ \upsilon & ::= & \tt{0} | \tt{1} | \tt{2} | ... \end{matrix}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}&space;\sigma&space;&&space;::=&space;&&space;{\tt&space;O}\\&space;&&space;|&space;&&space;\sigma&space;\rightarrow&space;\sigma&space;\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}&space;\sigma&space;&&space;::=&space;&&space;{\tt&space;O}\\&space;&&space;|&space;&&space;\sigma&space;\rightarrow&space;\sigma&space;\end{matrix}" title="\begin{matrix} \sigma & ::= & {\tt O}\\ & | & \sigma \rightarrow \sigma \end{matrix}" /></a>

However we adopt standard bracketing conventions to eliminate ambiguity in the parser. Concretly, the parser implements the non-ambiguous grammar as follows:

<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}&space;&&\\&space;\mathbf{\tau}&&space;::=&space;&&space;\lambda&space;\mathbf{\upsilon}\tt{:}\sigma&space;.&space;\mathbf{\tau}\\&space;&&space;|&space;&&space;\alpha\\&space;&&\\&space;\alpha&space;&&space;::=&space;&&space;\beta&space;\\&space;&|&space;&\mathbf{\alpha\,&space;\tt{space}\,&space;\beta}&space;\\&space;&&\\&space;\beta&space;&&space;::=&space;&&space;\tt{(}\tau&space;\tt{)}\\&space;&|&&space;\upsilon&space;\\&space;&|&\tt{z}\\&space;&|&\tt{s}\\&space;&|&\tt{p}\\&space;&|&\tt{if}\\&space;&|&\tt{Y}\\&space;&&\\&space;\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}&space;&&\\&space;\mathbf{\tau}&&space;::=&space;&&space;\lambda&space;\mathbf{\upsilon}\tt{:}\sigma&space;.&space;\mathbf{\tau}\\&space;&&space;|&space;&&space;\alpha\\&space;&&\\&space;\alpha&space;&&space;::=&space;&&space;\beta&space;\\&space;&|&space;&\mathbf{\alpha\,&space;\tt{space}\,&space;\beta}&space;\\&space;&&\\&space;\beta&space;&&space;::=&space;&&space;\tt{(}\tau&space;\tt{)}\\&space;&|&&space;\upsilon&space;\\&space;&|&\tt{z}\\&space;&|&\tt{s}\\&space;&|&\tt{p}\\&space;&|&\tt{if}\\&space;&|&\tt{Y}\\&space;&&\\&space;\end{matrix}" title="\begin{matrix} &&\\ \mathbf{\tau}& ::= & \lambda \mathbf{\upsilon}\tt{:}\sigma . \mathbf{\tau}\\ & | & \alpha\\ &&\\ \alpha & ::= & \beta \\ &| &\mathbf{\alpha\, \tt{space}\, \beta} \\ &&\\ \beta & ::= & \tt{(}\tau \tt{)}\\ &|& \upsilon \\ &|&\tt{z}\\ &|&\tt{s}\\ &|&\tt{p}\\ &|&\tt{if}\\ &|&\tt{Y}\\ &&\\ \end{matrix}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}&space;\sigma&space;&&space;::=&space;&&space;\gamma&space;\\&space;&&space;|&space;&&space;\gamma&space;\tt{\rightarrow}&space;\sigma&space;\\&space;&&\\&space;\gamma&space;&&space;::=&&space;\tt{(}&space;\sigma&space;\tt{)}\,&space;|\,&space;\tt{Nat}&space;\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}&space;\sigma&space;&&space;::=&space;&&space;\gamma&space;\\&space;&&space;|&space;&&space;\gamma&space;\tt{\rightarrow}&space;\sigma&space;\\&space;&&\\&space;\gamma&space;&&space;::=&&space;\tt{(}&space;\sigma&space;\tt{)}\,&space;|\,&space;\tt{Nat}&space;\end{matrix}" title="\begin{matrix} \sigma & ::= & \gamma \\ & | & \gamma \tt{\rightarrow} \sigma \\ &&\\ \gamma & ::=& \tt{(} \sigma \tt{)}\, |\, \tt{Nat} \end{matrix}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=&&\\&space;\upsilon&space;&&space;::=&space;&&space;\tt{0}&space;|&space;\tt{1}&space;|&space;\tt{2}&space;|&space;...&space;\\" target="_blank"><img src="https://latex.codecogs.com/gif.latex?&&\\&space;\upsilon&space;&&space;::=&space;&&space;\tt{0}&space;|&space;\tt{1}&space;|&space;\tt{2}&space;|&space;...&space;\\" title="&&\\ \upsilon & ::= & \tt{0} | \tt{1} | \tt{2} | ... \\" /></a>

Some notes about the syntax:

- Variables are positive integers (including zero) as this is easy for Haskell to process, and for me implement variable generation. This is isomorphic to a whiteboard treatment using characters (like `\x:Nat.x`).
- Types are either literal `Nat` base types or nested arrow types: `T -> T`. Arrows associate to the right so that `Nat -> Nat -> Nat` is the same as `Nat -> (Nat -> Nat)` but not `((Nat -> Nat) -> Nat)`.
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

<a href="https://www.codecogs.com/eqnedit.php?latex=\overline{\Gamma&space;\vdash&space;z:Nat}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\overline{\Gamma&space;\vdash&space;z:Nat}" title="\overline{\Gamma \vdash z:Nat}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{&space;\Gamma&space;\vdash&space;n&space;:&space;Nat}{&space;\Gamma&space;\vdash&space;p\,&space;n&space;:&space;Nat}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{&space;\Gamma&space;\vdash&space;n&space;:&space;Nat}{&space;\Gamma&space;\vdash&space;p\,&space;n&space;:&space;Nat}" title="\frac{ \Gamma \vdash n : Nat}{ \Gamma \vdash p\, n : Nat}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{\Gamma&space;\vdash&space;n&space;:&space;Nat}{\Gamma&space;\vdash&space;s\,&space;n&space;:&space;Nat}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{\Gamma&space;\vdash&space;n&space;:&space;Nat}{\Gamma&space;\vdash&space;s\,&space;n&space;:&space;Nat}" title="\frac{\Gamma \vdash n : Nat}{\Gamma \vdash s\, n : Nat}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=p\,(s\,n)&space;\rightsquigarrow&space;n" target="_blank"><img src="https://latex.codecogs.com/gif.latex?p\,(s\,n)&space;\rightsquigarrow&space;n" title="p\,(s\,n) \rightsquigarrow n" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=p\,&space;z&space;\rightsquigarrow&space;z" target="_blank"><img src="https://latex.codecogs.com/gif.latex?p\,&space;z&space;\rightsquigarrow&space;z" title="p\, z \rightsquigarrow z" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{n&space;\rightsquigarrow&space;n'}{p\,&space;n&space;\rightsquigarrow&space;p\,n'}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{n&space;\rightsquigarrow&space;n'}{p\,&space;n&space;\rightsquigarrow&space;p\,n'}" title="\frac{n \rightsquigarrow n'}{p\, n \rightsquigarrow p\,n'}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{n&space;\rightsquigarrow&space;n'}{s\,&space;n&space;\rightsquigarrow&space;s\,n'}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{n&space;\rightsquigarrow&space;n'}{s\,&space;n&space;\rightsquigarrow&space;s\,n'}" title="\frac{n \rightsquigarrow n'}{s\, n \rightsquigarrow s\,n'}" /></a>

for if statements:

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{\Gamma&space;\vdash&space;n&space;:&space;Nat\quad&space;\Gamma&space;\vdash&space;t_{1}&space;:&space;T\quad&space;\Gamma&space;\vdash&space;t_{2}&space;:&space;T}{\Gamma&space;\vdash&space;if\,n\,t_{1}\,t_{2}&space;:&space;T}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{\Gamma&space;\vdash&space;n&space;:&space;Nat\quad&space;\Gamma&space;\vdash&space;t_{1}&space;:&space;T\quad&space;\Gamma&space;\vdash&space;t_{2}&space;:&space;T}{\Gamma&space;\vdash&space;if\,n\,t_{1}\,t_{2}&space;:&space;T}" title="\frac{\Gamma \vdash n : Nat\quad \Gamma \vdash t_{1} : T\quad \Gamma \vdash t_{2} : T}{\Gamma \vdash if\,n\,t_{1}\,t_{2} : T}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=if\,z\,M\,N&space;\rightsquigarrow&space;M&space;\newline&space;if\,(s\,&space;n)\,M\,N&space;\rightsquigarrow&space;N" target="_blank"><img src="https://latex.codecogs.com/gif.latex?if\,z\,M\,N&space;\rightsquigarrow&space;M&space;\newline&space;if\,(s\,&space;n)\,M\,N&space;\rightsquigarrow&space;N" title="if\,z\,M\,N \rightsquigarrow M \newline if\,(s\, n)\,M\,N \rightsquigarrow N" /></a>

and there are special elimination and reduction rules for general recursion:

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{\Gamma&space;\vdash&space;t&space;:&space;T&space;\rightarrow&space;T}{\Gamma&space;\vdash&space;Y\,t:T}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{\Gamma&space;\vdash&space;t&space;:&space;T&space;\rightarrow&space;T}{\Gamma&space;\vdash&space;Y\,t:T}" title="\frac{\Gamma \vdash t : T \rightarrow T}{\Gamma \vdash Y\,t:T}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=Y\,(\lambda&space;x&space;:&space;t&space;.&space;M)&space;\rightsquigarrow&space;M[x&space;:=&space;Y\,(\lambda&space;x:t.M)]" target="_blank"><img src="https://latex.codecogs.com/gif.latex?Y\,(\lambda&space;x&space;:&space;t&space;.&space;M)&space;\rightsquigarrow&space;M[x&space;:=&space;Y\,(\lambda&space;x:t.M)]" title="Y\,(\lambda x : t . M) \rightsquigarrow M[x := Y\,(\lambda x:t.M)]" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{f&space;\rightsquigarrow&space;f'}{Y\,&space;f&space;\rightsquigarrow&space;Y\,f'}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{f&space;\rightsquigarrow&space;f'}{Y\,&space;f&space;\rightsquigarrow&space;Y\,f'}" title="\frac{f \rightsquigarrow f'}{Y\, f \rightsquigarrow Y\,f'}" /></a>

or in other words:

<a href="https://www.codecogs.com/eqnedit.php?latex=Y\,&space;f&space;\rightsquigarrow\,&space;f\,&space;(Y\,&space;f)" target="_blank"><img src="https://latex.codecogs.com/gif.latex?Y\,&space;f&space;\rightsquigarrow\,&space;f\,&space;(Y\,&space;f)" title="Y\, f \rightsquigarrow\, f\, (Y\, f)" /></a>

- This implementation follows a [small-step](https://cs.stackexchange.com/questions/43294/difference-between-small-and-big-step-operational-semantics) operational semantics and Berendregt's [variable convention](https://cs.stackexchange.com/questions/69323/barendregts-variable-convention-what-does-it-mean) (see `substitution` in PCF.hs). 
- Reductions include the one-step reduction (see `reduce1` in PCF.hs), the many-step reduction (see `reduce` in PCF.hs). 

## Other Implementation Details
- PCF.hs contains the Haskell implementation of the calculus, including substitution, reduction, and other useful things.
- Parser.hs contains the monadic parser combinators needed to parse input strings into typed-term ASTs for the calculus.
- Repl.hs contains a simple read-eval-print loop which hooks into main, and into the parser.
- Main.hs is needed for GHC to compile without any flags, it also invokes the repl.

For contributions, see the project to-do list or submit a PR with something you think it needs.




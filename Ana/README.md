# The Ana(morphic) Programming Language
Haskell implementation of the simply typed lambda calculus with coinductive types. It ranges over base types, has functions, products, sums, units, and a special `ν` type (with a `ν` variable `X`) to define [corecursive types](). 

Coinductive types are an important stepping-stone towards [algebraic datatypes](https://en.wikipedia.org/wiki/Algebraic_data_type) in functional languages and a powerful tool to model infinte data such as [streams](https://en.wikipedia.org/wiki/Coinduction) in a strongly normalizing setting. Additionally, anamorphisms in this language form canonical functional [unfolds](http://www.cs.nott.ac.uk/~pszgmh/when.pdf).

## Prerequisites
You need [Haskell](https://www.haskell.org/), this compiles with GHC 8.2.2 at least (Stack resolver: lts-11.0).

## To Build & Run

To compile and run do:
`ghc -O2 -o ana Main`
then run `./ana`

Alternatively to use the GHCi Interpreter do:
`ghci Main`
then type `main`

In either case you get something like the following:
```
Welcome to the Ana REPL
Type some terms or press Enter to leave.
>
```

Note: When run in GHCi, you don't have the luxury of escaped characters, backspace, delete etc...
Compile it using GHC if you need this.

## Examples 
Where you can then have some fun, try these examples:
- `\x:A.x` this is the identity function on As.
- `\s:N(X*A).s` this is the identity function on streams.
- `case (inl () : 1 + A) (\x:1.x) (\y:A.())` (performs case analysis on the first argument to `case`, passing the result to the first function if `inl` or otherwise `inr`).
- `\s:N(X*A).snd (out s)` this gets the head of a stream by unfolding the stream once and then getting out the second element.

The parser is also smart enough to recognise λ, so you can copy and paste from the output:
```
>   \x:A.x
=   λx:A.x
>   λx:A.x
=   λx:A.x
```
`>` denotes the REPL waiting for input, `=` means no reductions occurred (it's the same term), `~>` denotes one reduction, and `~>*` denotes 0 or more reductions (although in practice this is 1 or more due to `=`).

Note: see syntax below for a description of what the various symbols mean.

There is also a reduction tracer, which should print each reduction step. prefix any string with `'` in order to see the reductions:
```
TODO
```

There is also a typing mechanism, which should display the type or fail as usual.
```
>   t\s:N(X*A).snd (out s)
ν(X × A)->ν(X × A)
>   t\x:X.x x
Cannot Type Term: \x:X.x x
```
If you provide a untypeable term, the type checker will fail and reduction will not occur.

You can save terms for the life of the program with a `let` expression. Any time a saved variable appears in a term, it will be substituted for the saved term:
```
TODO
```
Note: Consequently `let` and `=` are keywords, and so you cannot name variables with these. Additionally `inl`, `inr`, `ana`, `out`, `case`, `fst`, `snd`, and `X` are keywords in Cata.

Additionally we have type level `lett` statements that allow you to define and use types:
```
TODO
```
This makes it easier to define both terms and types, but does not allow type level application (See Omega). `lett` is also a keyword.

## Syntax 

We base the language on the BNF for Ana:

<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}&space;\mathbf{\tau}&&space;::=&space;&&space;\lambda&space;\mathbf{\upsilon}{\tt&space;:}\sigma&space;.&space;\mathbf{\tau}\\&space;&&space;|&space;&&space;\tau\,&space;\tau\\&space;&&space;|&space;&&space;\upsilon&space;\\&space;&|&&space;out\,\tau\\&space;&|&&space;(ana\,\tau:\sigma)\,\tau\\&space;&|&&space;inl\,\tau:\sigma\\&space;&|&&space;inr\,\tau:\sigma\\&space;&|&&space;case\,\tau\,\tau\,\tau\\&space;&|&&space;(\tau,&space;\tau)\\&space;&|&&space;\pi_{1}\,\tau\\&space;&|&&space;\pi_{2}\,\tau\\&space;&|&&space;()&space;\\&space;\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}&space;\mathbf{\tau}&&space;::=&space;&&space;\lambda&space;\mathbf{\upsilon}{\tt&space;:}\sigma&space;.&space;\mathbf{\tau}\\&space;&&space;|&space;&&space;\tau\,&space;\tau\\&space;&&space;|&space;&&space;\upsilon&space;\\&space;&|&&space;out\,\tau\\&space;&|&&space;(ana\,\tau:\sigma)\,\tau\\&space;&|&&space;inl\,\tau:\sigma\\&space;&|&&space;inr\,\tau:\sigma\\&space;&|&&space;case\,\tau\,\tau\,\tau\\&space;&|&&space;(\tau,&space;\tau)\\&space;&|&&space;\pi_{1}\,\tau\\&space;&|&&space;\pi_{2}\,\tau\\&space;&|&&space;()&space;\\&space;\end{matrix}" title="\begin{matrix} \mathbf{\tau}& ::= & \lambda \mathbf{\upsilon}{\tt :}\sigma . \mathbf{\tau}\\ & | & \tau\, \tau\\ & | & \upsilon \\ &|& out\,\tau\\ &|& (ana\,\tau:\sigma)\,\tau\\ &|& inl\,\tau:\sigma\\ &|& inr\,\tau:\sigma\\ &|& case\,\tau\,\tau\,\tau\\ &|& (\tau, \tau)\\ &|& \pi_{1}\,\tau\\ &|& \pi_{2}\,\tau\\ &|& () \\ \end{matrix}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\upsilon&space;&&space;::=&space;&&space;\tt{a}&space;|&space;\tt{b}&space;|&space;\tt{c}&space;|&space;...&space;|&space;\tt{aa}&space;|&space;...&space;\\" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\upsilon&space;&&space;::=&space;&&space;\tt{a}&space;|&space;\tt{b}&space;|&space;\tt{c}&space;|&space;...&space;|&space;\tt{aa}&space;|&space;...&space;\\" title="\upsilon & ::= & \tt{a} | \tt{b} | \tt{c} | ... | \tt{aa} | ... \\" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}&space;\sigma&space;&&space;::=&space;&&space;A,B,C,...\\&space;&&space;|&space;&&space;\sigma&space;\rightarrow&space;\sigma&space;\\&space;&|&&space;\top\\&space;&|&&space;\sigma&space;\times&space;\sigma\\&space;&|&&space;\sigma&space;&plus;&space;\sigma\\&space;&|&&space;\nu\,&space;\sigma\\&space;&|&&space;{\tt&space;X}&space;\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}&space;\sigma&space;&&space;::=&space;&&space;A,B,C,...\\&space;&&space;|&space;&&space;\sigma&space;\rightarrow&space;\sigma&space;\\&space;&|&&space;\top\\&space;&|&&space;\sigma&space;\times&space;\sigma\\&space;&|&&space;\sigma&space;&plus;&space;\sigma\\&space;&|&&space;\nu\,&space;\sigma\\&space;&|&&space;{\tt&space;X}&space;\end{matrix}" title="\begin{matrix} \sigma & ::= & A,B,C,...\\ & | & \sigma \rightarrow \sigma \\ &|& \top\\ &|& \sigma \times \sigma\\ &|& \sigma + \sigma\\ &|& \nu\, \sigma\\ &|& {\tt X} \end{matrix}" /></a>

However we adopt standard bracketing conventions to eliminate ambiguity in the parser. Concretely, the parser implements the non-ambiguous grammar as follows:

<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}&space;&&\\&space;\mathbf{\tau}&&space;::=&space;&&space;\lambda&space;\mathbf{\upsilon}\tt{:}\sigma&space;.&space;\mathbf{\tau}\\&space;&&space;|&space;&&space;\alpha\\&space;&&\\&space;\alpha&space;&&space;::=&space;&&space;\beta&space;\\&space;&|&space;&\mathbf{\alpha\,&space;\tt{space}\,&space;\beta}&space;\\&space;&&\\&space;\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}&space;&&\\&space;\mathbf{\tau}&&space;::=&space;&&space;\lambda&space;\mathbf{\upsilon}\tt{:}\sigma&space;.&space;\mathbf{\tau}\\&space;&&space;|&space;&&space;\alpha\\&space;&&\\&space;\alpha&space;&&space;::=&space;&&space;\beta&space;\\&space;&|&space;&\mathbf{\alpha\,&space;\tt{space}\,&space;\beta}&space;\\&space;&&\\&space;\end{matrix}" title="\begin{matrix} &&\\ \mathbf{\tau}& ::= & \lambda \mathbf{\upsilon}\tt{:}\sigma . \mathbf{\tau}\\ & | & \alpha\\ &&\\ \alpha & ::= & \beta \\ &| &\mathbf{\alpha\, \tt{space}\, \beta} \\ &&\\ \end{matrix}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}&&\\&space;\beta&space;&&space;::=&space;&&space;\tt{(}\tau&space;\tt{)}\\&space;&|&&space;\upsilon&space;\\&space;&|&&space;()&space;\\&space;&|&&space;out\\&space;&|&&space;ana\,\tau&space;:&space;\sigma\\&space;&|&&space;(\tau,\tau)\\&space;&|&&space;\pi_{1}\,\tau\\&space;&|&&space;\pi_{2}\,\tau\\&space;&|&&space;inl\,\tau:\sigma\\&space;&|&&space;inr\,\tau:\sigma\\&space;&|&&space;case&space;\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}&&\\&space;\beta&space;&&space;::=&space;&&space;\tt{(}\tau&space;\tt{)}\\&space;&|&&space;\upsilon&space;\\&space;&|&&space;()&space;\\&space;&|&&space;out\\&space;&|&&space;ana\,\tau&space;:&space;\sigma\\&space;&|&&space;(\tau,\tau)\\&space;&|&&space;\pi_{1}\,\tau\\&space;&|&&space;\pi_{2}\,\tau\\&space;&|&&space;inl\,\tau:\sigma\\&space;&|&&space;inr\,\tau:\sigma\\&space;&|&&space;case&space;\end{matrix}" title="\begin{matrix}&&\\ \beta & ::= & \tt{(}\tau \tt{)}\\ &|& \upsilon \\ &|& () \\ &|& out\\ &|& ana\,\tau : \sigma\\ &|& (\tau,\tau)\\ &|& \pi_{1}\,\tau\\ &|& \pi_{2}\,\tau\\ &|& inl\,\tau:\sigma\\ &|& inr\,\tau:\sigma\\ &|& case \end{matrix}" /></a>

and types:

<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}&space;\sigma&space;&&space;::=&space;&&space;\gamma&space;\\&space;&&space;|&space;&&space;\gamma&space;\tt{\rightarrow}&space;\sigma&space;\\&space;&&\\&space;\gamma&space;&&space;::=&&space;\delta&space;&plus;&space;\delta\\&space;&|&\delta\\&&\\&space;\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}&space;\sigma&space;&&space;::=&space;&&space;\gamma&space;\\&space;&&space;|&space;&&space;\gamma&space;\tt{\rightarrow}&space;\sigma&space;\\&space;&&\\&space;\gamma&space;&&space;::=&&space;\delta&space;&plus;&space;\delta\\&space;&|&\delta\\&&\\&space;\end{matrix}" title="\begin{matrix} \sigma & ::= & \gamma \\ & | & \gamma \tt{\rightarrow} \sigma \\ &&\\ \gamma & ::=& \delta + \delta\\ &|&\delta\\&&\\ \end{matrix}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}\delta&space;&::=&&space;\rho&space;\times&space;\rho\\&space;&|&&space;\rho\\&&\\&space;\rho&space;&&space;::=&&space;\tt{(}&space;\sigma&space;\tt{)}\\&space;&|&&space;{\tt&space;X}\\&space;&|&&space;A,B,C,...\\&space;&|&&space;\top&space;\\&space;&|&&space;\nu\,(\sigma)&space;\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}\delta&space;&::=&&space;\rho&space;\times&space;\rho\\&space;&|&&space;\rho\\&&\\&space;\rho&space;&&space;::=&&space;\tt{(}&space;\sigma&space;\tt{)}\\&space;&|&&space;{\tt&space;X}\\&space;&|&&space;A,B,C,...\\&space;&|&&space;\top&space;\\&space;&|&&space;\nu\,(\sigma)&space;\end{matrix}" title="\begin{matrix}\delta &::=& \rho \times \rho\\ &|& \rho\\&&\\ \rho & ::=& \tt{(} \sigma \tt{)}\\ &|& {\tt X}\\ &|& A,B,C,...\\ &|& \top \\ &|& \nu\,(\sigma) \end{matrix}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}&space;\upsilon&space;&&space;::=&space;&&space;\tt{0}&space;|&space;\tt{1}&space;|&space;\tt{2}&space;|&space;...&space;\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}&space;\upsilon&space;&&space;::=&space;&&space;\tt{0}&space;|&space;\tt{1}&space;|&space;\tt{2}&space;|&space;...&space;\end{matrix}" title="\begin{matrix} \upsilon & ::= & \tt{0} | \tt{1} | \tt{2} | ... \end{matrix}" /></a>

Some notes about the syntax:

- The above syntax only covers the core calculus, and not the repl extensions (such as let bindings above). The extensions are simply added on in the repl.
- Variables are strings (excluding numbers), as this is isomorphic to a whiteboard treatment and hence the most familiar.
- `X` is a reserved `ν` variable and is interpreted as such in a term. _ν-less_ terms that use `X` won't act differently than STLC but introducing `ν` will change the semantics. Best to avoid unless using `ν`. 
- Types range over upper-case characters `A,B,C...`, nested arrow types: `T -> T`, product types `A * B` (or `×`), sum types `A + B`, unit types `1` (or `⊤`), a special Nu type `ν(T)` (or `N(T)`), and Nu type variable `X`.  
- Arrows associate to the right so that `A -> A -> A` is the same as `A -> (A -> A)` but not `((A -> A) -> A)`. Similar rules follow for the other types.
- Products have the highest precedence, followed by sums, arrows, and then all other types. 
- Nested terms don't need brackets: `\x:A.\y:B. y` unless enforcing application on the right. Whitespace does not matter `(\x:A.          x)` unless it is between application where you need at least one space.
- Products are 2-element pairs like in Haskell. You form products like `(x, y)` and access each element using `fst` (or `π1`) and `snd` (or `π2`).
- Sums are 2-element co-pairs, formed with either `inl a:A` or `inr b:B` (assuming either `a:A` or `b:B` is in scope). `case s f g` does case analysis on `s`, passing the result to the function `f` if s was `inl` or `g` if `inr`.
- Units are largely uninteresting, but can be formed as `()` like in Haskell, and typed like `1` or (or `⊤`).
- To construct terms using `ana` or `case`, use _space_ to apply arguments. for instance, `case s f c` applies `s` to `case`, which then applies `f` to the `case s` and `g` to `case s f` etc...
- Coinductive types are formed by prefixing a term with `out` and a type with `N` (or `ν`). Concretely, a coinductive term looks like `out x:t` where `x` is the term and `t` is its type. Similarly, a coinductive type looks like `ν(T)` where `T` is the type we insist is coinductive. 

TODO example

See the semantics for a description of what these constructions mean.
- I should note that the parser is pretty verbose with its types, a nice challenge would be to implement a backtracking parser as a means to reduce the number of necessary type annotations (in say the example above).
- To quit use `Ctrl+C` or whatever your machine uses to interrupt computations.

## Semantics

The semantics implements full beta-reduction on terms and alpha-equivalence as the `Eq` instance of `AnaTerm`. The semantics are the same as the STLC but with additional rules for `out`, `ana`, and all the machinery for products, sums and units. We reformulate the semantics as [typing judgements](https://existentialtype.wordpress.com/2011/03/27/the-holy-trinity/):

for variables (adopted from STLC):

<a href="https://www.codecogs.com/eqnedit.php?latex=\overline{\Gamma&space;\vdash&space;x:T},\quad&space;\mbox{(if&space;$x:T&space;\in&space;\Gamma$)}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\overline{\Gamma&space;\vdash&space;x:T},\quad&space;\mbox{(if&space;$x:T&space;\in&space;\Gamma$)}" title="\overline{\Gamma \vdash x:T},\quad \mbox{(if $x:T \in \Gamma$)}" /></a>

for abstractions (adopted from STLC):

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{\Gamma&space;,x:A&space;\vdash&space;t:B}{\Gamma&space;\vdash&space;(\lambda&space;x&space;:&space;A.&space;t)&space;:&space;B&space;}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{\Gamma&space;,x:A&space;\vdash&space;t:B}{\Gamma&space;\vdash&space;(\lambda&space;x&space;:&space;A.&space;t)&space;:&space;B&space;}" title="\frac{\Gamma ,x:A \vdash t:B}{\Gamma \vdash (\lambda x : A. t) : B }" /></a>

and application (adopted from STLC):

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{\Gamma&space;\vdash&space;f&space;:&space;A&space;\Rightarrow&space;B\quad&space;\Gamma&space;\vdash&space;x&space;:&space;A}{\Gamma&space;\vdash&space;(f&space;x)&space;:&space;B}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{\Gamma&space;\vdash&space;f&space;:&space;A&space;\Rightarrow&space;B\quad&space;\Gamma&space;\vdash&space;x&space;:&space;A}{\Gamma&space;\vdash&space;(f&space;x)&space;:&space;B}" title="\frac{\Gamma \vdash f : A \Rightarrow B\quad \Gamma \vdash x : A}{\Gamma \vdash (f x) : B}" /></a>

beta reduction (adopted from STLC):

<a href="https://www.codecogs.com/eqnedit.php?latex=(\lambda&space;x&space;:&space;T&space;.&space;M)N&space;\rightsquigarrow&space;M&space;[x&space;:=&space;N]" target="_blank"><img src="https://latex.codecogs.com/gif.latex?(\lambda&space;x&space;:&space;T&space;.&space;M)N&space;\rightsquigarrow&space;M&space;[x&space;:=&space;N]" title="(\lambda x : T . M)N \rightsquigarrow M [x := N]" /></a>

We have special introduction, elimination and reduction rules for products:

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{\Gamma&space;\vdash&space;a&space;:&space;A\quad&space;\Gamma&space;\vdash&space;b&space;:&space;B}{\Gamma&space;\vdash&space;(a,b):A\times&space;B}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{\Gamma&space;\vdash&space;a&space;:&space;A\quad&space;\Gamma&space;\vdash&space;b&space;:&space;B}{\Gamma&space;\vdash&space;(a,b):A\times&space;B}" title="\frac{\Gamma \vdash a : A\quad \Gamma \vdash b : B}{\Gamma \vdash (a,b):A\times B}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{\Gamma&space;\vdash&space;p:A\times&space;B}{\Gamma&space;\vdash&space;\pi_{1}\,p:A}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{\Gamma&space;\vdash&space;p:A\times&space;B}{\Gamma&space;\vdash&space;\pi_{1}\,p:A}" title="\frac{\Gamma \vdash p:A\times B}{\Gamma \vdash \pi_{1}\,p:A}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{\Gamma&space;\vdash&space;p:A\times&space;B}{\Gamma&space;\vdash&space;\pi_{2}\,p:B}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{\Gamma&space;\vdash&space;p:A\times&space;B}{\Gamma&space;\vdash&space;\pi_{2}\,p:B}" title="\frac{\Gamma \vdash p:A\times B}{\Gamma \vdash \pi_{2}\,p:B}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\pi_{1}(a,b)&space;\rightsquigarrow&space;a" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\pi_{1}(a,b)&space;\rightsquigarrow&space;a" title="\pi_{1}(a,b) \rightsquigarrow a" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\pi_{2}(a,b)&space;\rightsquigarrow&space;b" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\pi_{2}(a,b)&space;\rightsquigarrow&space;b" title="\pi_{2}(a,b) \rightsquigarrow b" /></a>

for sums:

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{\Gamma&space;\vdash&space;a:A}{\Gamma&space;\vdash&space;inl\,a&space;:&space;A&space;&plus;&space;B}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{\Gamma&space;\vdash&space;a:A}{\Gamma&space;\vdash&space;inl\,a&space;:&space;A&space;&plus;&space;B}" title="\frac{\Gamma \vdash a:A}{\Gamma \vdash inl\,a : A + B}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{\Gamma&space;\vdash&space;b:B}{\Gamma&space;\vdash&space;inr\,b&space;:&space;A&space;&plus;&space;B}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{\Gamma&space;\vdash&space;b:B}{\Gamma&space;\vdash&space;inr\,b&space;:&space;A&space;&plus;&space;B}" title="\frac{\Gamma \vdash b:B}{\Gamma \vdash inr\,b : A + B}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{\Gamma&space;\vdash&space;s:&space;A&plus;B\quad\Gamma\vdash&space;f&space;:A\rightarrow&space;X\quad\Gamma\vdash&space;g:B\rightarrow&space;X}{\Gamma&space;\vdash&space;case\,&space;s\,&space;f\,&space;g:X}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{\Gamma&space;\vdash&space;s:&space;A&plus;B\quad\Gamma\vdash&space;f&space;:A\rightarrow&space;X\quad\Gamma\vdash&space;g:B\rightarrow&space;X}{\Gamma&space;\vdash&space;case\,&space;s\,&space;f\,&space;g:X}" title="\frac{\Gamma \vdash s: A+B\quad\Gamma\vdash f :A\rightarrow X\quad\Gamma\vdash g:B\rightarrow X}{\Gamma \vdash case\, s\, f\, g:X}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=case\,(inl\,&space;a)\,f\,g\rightsquigarrow\,f\,a" target="_blank"><img src="https://latex.codecogs.com/gif.latex?case\,(inl\,&space;a)\,f\,g\rightsquigarrow\,f\,a" title="case\,(inl\, a)\,f\,g\rightsquigarrow\,f\,a" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=case\,(inr\,&space;b)\,f\,g\rightsquigarrow\,g\,b" target="_blank"><img src="https://latex.codecogs.com/gif.latex?case\,(inr\,&space;b)\,f\,g\rightsquigarrow\,g\,b" title="case\,(inr\, b)\,f\,g\rightsquigarrow\,g\,b" /></a>

and for units:

<a href="https://www.codecogs.com/eqnedit.php?latex=\overline{\Gamma\vdash&space;()&space;:&space;\top}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\overline{\Gamma\vdash&space;()&space;:&space;\top}" title="\overline{\Gamma\vdash () : \top}" /></a>

with the obvious rules for reduction inside each term that has a subterm. Finally, we have rules for `ν` type introduction, elimination and, reduction.

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{\Gamma\vdash&space;t&space;:&space;\nu&space;F}{\Gamma\vdash&space;out\,&space;t&space;:&space;F(\nu\,F)}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{\Gamma\vdash&space;t&space;:&space;\nu&space;F}{\Gamma\vdash&space;out\,&space;t&space;:&space;F(\nu\,F)}" title="\frac{\Gamma\vdash t : \nu F}{\Gamma\vdash out\, t : F(\nu\,F)}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{\Gamma\vdash&space;f&space;:&space;X\rightarrow&space;F\,X}{\Gamma\vdash\,ana\,f:X&space;\rightarrow&space;\nu&space;F}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{\Gamma\vdash&space;f&space;:&space;X\rightarrow&space;F\,X}{\Gamma\vdash\,ana\,f:X&space;\rightarrow&space;\nu&space;F}" title="\frac{\Gamma\vdash f : X\rightarrow F\,X}{\Gamma\vdash\,ana\,f:X \rightarrow \nu F}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=out\,(ana\,f\,t)&space;\rightsquigarrow\,F\,(ana\,&space;f)\,(f\,t)" target="_blank"><img src="https://latex.codecogs.com/gif.latex?out\,(ana\,f\,t)&space;\rightsquigarrow\,F\,(ana\,&space;f)\,(f\,t)" title="out\,(ana\,f\,t) \rightsquigarrow\,F\,(ana\, f)\,(f\,t)" /></a>

with reduction inside terms like above.

- The rules for variables, abstraction and application follow directly from STLC; the rules for products, sums, and units are largely standard in the literature. ν types in the [literature](https://www.cis.upenn.edu/~bcpierce/tapl/) are typically written as `νX.T` where T is a type that contains X. Since we only use one ν variable, we just write `T` (submit a PR with the more general form!)
- We adopt the categorical notation of a [functor](https://en.wikipedia.org/wiki/Functor), which expresses a mapping from terms to terms and types to types. For example, given the functor type `F X = 1 + X`, and type `Y = B`, then `F Y = 1 + Y = 1 + B`. similarly if we had a term `inl _ : F _`, then `inl b: 1+B` for some `b:B`. This idea translates directly into Ana, however we don't do type-level application, and so you must apply these types yourself before using them. Concretely, you might provide `F Y` as the type `1 + B` in Ana.
- We can construct functors using `+`, `*`, `->`. A functor is _strictly positive_ if it contains only constant variables `A,B,C,...`, μ variable `X`, `+`, `*`, and `->` such that `X` only occurs on the right of arrows. For every _s.p._ functor F, we can define a coinductive type `νF`. A strictly positive check has not been implemented yet, behaviour for non-s.p functors is thus undefined (submit a PR, as it's an easy fix). 
- TODO Example
- TODO Update We can implement functions on functors too, supposing `F X = 1 + X`, then a function `F X -> X` might [reduce](https://www.schoolofhaskell.com/user/bartosz/understanding-algebras) F-shaped terms into a simpler form `X`. For instance, `\x:1+X.case x (\x:1.()) (\x:X.())` reduces terms of type `1+X` to `()`; in languages with useful types (like booleans), we might use this to implement more powerful constructs (like conditionals). 
- We can extend the notation of typed unfolding by _lifting_ it to the coinductive level, that is given `f:X -> F X`, we get `ana f:X -> νF`. This forms the elimination rule for coinductive types and is known as the _anamorphism_ of F.
- We combine our knowledge of anamorphisms and coinductive types and get unfolds (finally!). TODO FIX 
- This implementation follows a [small-step](https://cs.stackexchange.com/questions/43294/difference-between-small-and-big-step-operational-semantics) operational semantics and Berendregt's [variable convention](https://cs.stackexchange.com/questions/69323/barendregts-variable-convention-what-does-it-mean) (see `substitution` in Ana.hs). 
- Reductions include the one-step reduction (see `reduce1` in Ana.hs), the many-step reduction (see `reduce` in Ana.hs). 

## Other Implementation Details
- Ana.hs contains the Haskell implementation of the calculus, including substitution, reduction, and other useful things.
- Parser.hs contains the monadic parser combinators needed to parse input strings into typed-term ASTs for the calculus.
- Repl.hs contains a simple read-eval-print loop which hooks into main, and into the parser.
- Main.hs is needed for GHC to compile without any flags, it also invokes the repl.

For contributions, see the project to-do list or submit a PR with something you think it needs.


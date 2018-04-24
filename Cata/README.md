# The Cata(morphic) Programming Language
Haskell implementation of the simply typed lambda calculus with inductive types. It ranges over base types, has functions, products, sums, units, and a special `μ` type (with a `μ` variable `X`) to define [recursive types](https://en.wikipedia.org/wiki/Recursive_data_type). It is strongly normalizing due to the well-founded nature of inductive types but not Turing Complete.

Inductive types are an important stepping-stone towards [algebraic datatypes](https://en.wikipedia.org/wiki/Algebraic_data_type) in functional languages and a powerful tool to use with [inductive equational reasoning](https://en.wikipedia.org/wiki/Structural_induction). Additionally, catamorphisms in this language form canonical functional [folds](https://en.wikipedia.org/wiki/Fold_(higher-order_function)).

## Prerequisites
You need [Haskell](https://www.haskell.org/), this compiles with GHC 8.2.2 at least (Stack resolver: lts-11.0).

## To Build & Run

To compile and run do:
`ghc -O2 -o cata Main`
then run `./cata`

Alternatively to use the GHCi Interpreter do:
`ghci Main`
then type `main`

In either case you get something like the following:
```
Welcome to the Cata REPL
Type some terms or press Enter to leave.
>
```

Note: When run in GHCi, you don't have the luxury of escaped characters, backspace, delete etc...
Compile it using GHC if you need this.

## Examples 
Where you can then have some fun, try these examples:
- `\1:A.1`
- `in (inl (): 1 + M (1+X)):1+M(1+X)` (the encoding of [peano](https://wiki.haskell.org/Peano_numbers) style _zero_)
- `\1:M(1+((1*X)*X)).1` (the identity function for a binary tree, with nil leaves and elements stored internally)
- `case (inr ():1+A) (\1:1.1) (\2:A.())` (performs case analysis on the first argument to `case`, passing the result to the first function if `inl` or otherwise `inr`).
- `\1:A.\2:B.snd (1,2)`.
- `\0:1+M(1+X). in (inr 0:1+M(1+X)):1+M(1+X)` the successor function for inductively defined nats.

The parser is also smart enough to recognise λ, so you can copy and paste from the output:
```
> \1:A.1
λ1:A.1
> λ1:A.1
λ1:A.1
```
Note: see syntax below for a description of what the various symbols mean.

There is also a reduction tracer, which should print each reduction step. prefix any string with `'` in order to see the reductions:
```
TODO
```

There is also a typing mechanism, which should display the type or fail as usual.
```
> t(\0:1+M(1+X). in (inr 0:1+M(1+X)):1+M(1+X)) (in (inl (): 1 + M (1+X)):1+M(1+X))
⊤ + μ(⊤ + X)
> t\0:A -> A. 0 0
Cannot Type Term: \0:A -> A. 0 0
>
```
Note: The above terms denote _succ 0_ and self-application respectively. If you provide a un-typeable term, the type checker will fail and reduction will not occur.

## Syntax 

We base the language on the BNF for Cata:

<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}&space;\mathbf{\tau}&&space;::=&space;&&space;\lambda&space;\mathbf{\upsilon}{\tt&space;:}\sigma&space;.&space;\mathbf{\tau}\\&space;&&space;|&space;&&space;\tau\,&space;\tau\\&space;&&space;|&space;&&space;\upsilon&space;\\&space;&|&&space;in\,\tau:\sigma\\&space;&|&&space;cata\,\tau\,\tau\\&space;&|&&space;inl\,\tau:\sigma\\&space;&|&&space;inr\,\tau:\sigma\\&space;&|&&space;case\,\tau\,\tau\,\tau\\&space;&|&&space;(\tau,&space;\tau)\\&space;&|&&space;\pi_{1}\,\tau\\&space;&|&&space;\pi_{2}\,\tau\\&space;&|&&space;()&space;\\&space;&&\\&space;\upsilon&space;&&space;::=&space;&&space;\tt{0}&space;|&space;\tt{1}&space;|&space;\tt{2}&space;|&space;...&space;\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}&space;\mathbf{\tau}&&space;::=&space;&&space;\lambda&space;\mathbf{\upsilon}{\tt&space;:}\sigma&space;.&space;\mathbf{\tau}\\&space;&&space;|&space;&&space;\tau\,&space;\tau\\&space;&&space;|&space;&&space;\upsilon&space;\\&space;&|&&space;in\,\tau:\sigma\\&space;&|&&space;cata\,\tau\,\tau\\&space;&|&&space;inl\,\tau:\sigma\\&space;&|&&space;inr\,\tau:\sigma\\&space;&|&&space;case\,\tau\,\tau\,\tau\\&space;&|&&space;(\tau,&space;\tau)\\&space;&|&&space;\pi_{1}\,\tau\\&space;&|&&space;\pi_{2}\,\tau\\&space;&|&&space;()&space;\\&space;&&\\&space;\upsilon&space;&&space;::=&space;&&space;\tt{0}&space;|&space;\tt{1}&space;|&space;\tt{2}&space;|&space;...&space;\end{matrix}" title="\begin{matrix} \mathbf{\tau}& ::= & \lambda \mathbf{\upsilon}{\tt :}\sigma . \mathbf{\tau}\\ & | & \tau\, \tau\\ & | & \upsilon \\ &|& in\,\tau:\sigma\\ &|& cata\,\tau\,\tau\\ &|& inl\,\tau:\sigma\\ &|& inr\,\tau:\sigma\\ &|& case\,\tau\,\tau\,\tau\\ &|& (\tau, \tau)\\ &|& \pi_{1}\,\tau\\ &|& \pi_{2}\,\tau\\ &|& () \\ &&\\ \upsilon & ::= & \tt{0} | \tt{1} | \tt{2} | ... \end{matrix}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}&space;\sigma&space;&&space;::=&space;&&space;A,B,C,...\\&space;&&space;|&space;&&space;\sigma&space;\rightarrow&space;\sigma&space;\\&space;&|&&space;\top\\&space;&|&&space;\sigma&space;\times&space;\sigma\\&space;&|&&space;\sigma&space;&plus;&space;\sigma\\&space;&|&&space;\mu\,&space;\sigma\\&space;&|&&space;{\tt&space;X}&space;\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}&space;\sigma&space;&&space;::=&space;&&space;A,B,C,...\\&space;&&space;|&space;&&space;\sigma&space;\rightarrow&space;\sigma&space;\\&space;&|&&space;\top\\&space;&|&&space;\sigma&space;\times&space;\sigma\\&space;&|&&space;\sigma&space;&plus;&space;\sigma\\&space;&|&&space;\mu\,&space;\sigma\\&space;&|&&space;{\tt&space;X}&space;\end{matrix}" title="\begin{matrix} \sigma & ::= & A,B,C,...\\ & | & \sigma \rightarrow \sigma \\ &|& \top\\ &|& \sigma \times \sigma\\ &|& \sigma + \sigma\\ &|& \mu\, \sigma\\ &|& {\tt X} \end{matrix}" /></a>

However we adopt standard bracketing conventions to eliminate ambiguity in the parser. Concretely, the parser implements the non-ambiguous grammar as follows:

<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}&space;&&\\&space;\mathbf{\tau}&&space;::=&space;&&space;\lambda&space;\mathbf{\upsilon}\tt{:}\sigma&space;.&space;\mathbf{\tau}\\&space;&&space;|&space;&&space;\alpha\\&space;&&\\&space;\alpha&space;&&space;::=&space;&&space;\beta&space;\\&space;&|&space;&\mathbf{\alpha\,&space;\tt{space}\,&space;\beta}&space;\\&space;&&\\&space;\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}&space;&&\\&space;\mathbf{\tau}&&space;::=&space;&&space;\lambda&space;\mathbf{\upsilon}\tt{:}\sigma&space;.&space;\mathbf{\tau}\\&space;&&space;|&space;&&space;\alpha\\&space;&&\\&space;\alpha&space;&&space;::=&space;&&space;\beta&space;\\&space;&|&space;&\mathbf{\alpha\,&space;\tt{space}\,&space;\beta}&space;\\&space;&&\\&space;\end{matrix}" title="\begin{matrix} &&\\ \mathbf{\tau}& ::= & \lambda \mathbf{\upsilon}\tt{:}\sigma . \mathbf{\tau}\\ & | & \alpha\\ &&\\ \alpha & ::= & \beta \\ &| &\mathbf{\alpha\, \tt{space}\, \beta} \\ &&\\ \end{matrix}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}&&\\&space;\beta&space;&&space;::=&space;&&space;\tt{(}\tau&space;\tt{)}\\&space;&|&&space;\upsilon&space;\\&space;&|&&space;()&space;\\&space;&|&&space;in\,\tau:\sigma\\&space;&|&&space;cata\\&space;&|&&space;(\tau,\tau)\\&space;&|&&space;\pi_{1}\,\tau\\&space;&|&&space;\pi_{2}\,\tau\\&space;&|&&space;inl\,\tau:\sigma\\&space;&|&&space;inr\,\tau:\sigma\\&space;&|&&space;case&space;\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}&&\\&space;\beta&space;&&space;::=&space;&&space;\tt{(}\tau&space;\tt{)}\\&space;&|&&space;\upsilon&space;\\&space;&|&&space;()&space;\\&space;&|&&space;in\,\tau:\sigma\\&space;&|&&space;cata\\&space;&|&&space;(\tau,\tau)\\&space;&|&&space;\pi_{1}\,\tau\\&space;&|&&space;\pi_{2}\,\tau\\&space;&|&&space;inl\,\tau:\sigma\\&space;&|&&space;inr\,\tau:\sigma\\&space;&|&&space;case&space;\end{matrix}" title="\begin{matrix}&&\\ \beta & ::= & \tt{(}\tau \tt{)}\\ &|& \upsilon \\ &|& () \\ &|& in\,\tau:\sigma\\ &|& cata\\ &|& (\tau,\tau)\\ &|& \pi_{1}\,\tau\\ &|& \pi_{2}\,\tau\\ &|& inl\,\tau:\sigma\\ &|& inr\,\tau:\sigma\\ &|& case \end{matrix}" /></a>

and types:

<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}&space;\sigma&space;&&space;::=&space;&&space;\gamma&space;\\&space;&&space;|&space;&&space;\gamma&space;\tt{\rightarrow}&space;\sigma&space;\\&space;&&\\&space;\gamma&space;&&space;::=&&space;\delta&space;&plus;&space;\delta\\&space;&|&\delta\\&&\\&space;\delta&space;&::=&&space;\rho&space;\times&space;\rho\\&space;&|&&space;\rho\\&&\\&space;\rho&space;&&space;::=&&space;\tt{(}&space;\sigma&space;\tt{)}\\&space;&|&&space;{\tt&space;X}\\&space;&|&&space;A,B,C,...\\&space;&|&&space;\top&space;\\&space;&|&&space;\mu\,(\sigma)&space;\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}&space;\sigma&space;&&space;::=&space;&&space;\gamma&space;\\&space;&&space;|&space;&&space;\gamma&space;\tt{\rightarrow}&space;\sigma&space;\\&space;&&\\&space;\gamma&space;&&space;::=&&space;\delta&space;&plus;&space;\delta\\&space;&|&\delta\\&&\\&space;\delta&space;&::=&&space;\rho&space;\times&space;\rho\\&space;&|&&space;\rho\\&&\\&space;\rho&space;&&space;::=&&space;\tt{(}&space;\sigma&space;\tt{)}\\&space;&|&&space;{\tt&space;X}\\&space;&|&&space;A,B,C,...\\&space;&|&&space;\top&space;\\&space;&|&&space;\mu\,(\sigma)&space;\end{matrix}" title="\begin{matrix} \sigma & ::= & \gamma \\ & | & \gamma \tt{\rightarrow} \sigma \\ &&\\ \gamma & ::=& \delta + \delta\\ &|&\delta\\&&\\ \delta &::=& \rho \times \rho\\ &|& \rho\\&&\\ \rho & ::=& \tt{(} \sigma \tt{)}\\ &|& {\tt X}\\ &|& A,B,C,...\\ &|& \top \\ &|& \mu\,(\sigma) \end{matrix}" /></a>

Some notes about the syntax:

- Variables are positive integers (including zero) as this is easy for Haskell to process, and for me implement variable generation. This is isomorphic to a whiteboard treatment using characters (like `\x:A.x`).
- `X` is a reserved `μ` variable and is interpreted as such in a term. _μ-less_ terms that use `X` won't act differently than STLC but introducing `μ` will change the semantics. Best to avoid unless using `μ`. 
- Types range over upper-case characters `A,B,C...`, nested arrow types: `T -> T`, product types `A * B` (or `×`), sum types `A + B`, unit types `1` (or `⊤`), a special Mu type `μ(T)` (or `M(T)`), and Mu type variable `X`.  
- Arrows associate to the right so that `A -> A -> A` is the same as `A -> (A -> A)` but not `((A -> A) -> A)`. Similar rules follow for the other types.
- Products have the highest precedence, followed by sums, arrows, and then all other types. 
- Nested terms don't need brackets: `\1:A.\2:B. 2` unless enforcing application on the right. Whitespace does not matter `(\1:A.          1)` unless it is between application where you need at least one space.
- Products are 2-element pairs like in Haskell. You form products like `(1, 2)` and access each element using `fst` (or `π1`) and `snd` (or `π2`).
- Sums are 2-element co-pairs, formed with either `inl 1:A` or `inr 2:B` (assuming either `1:A` or `2:B` is in scope). `case s f g` does case analysis on `s`, passing the result to the function `f` if s was `inl` or `g` if `inr`.
- Units are largely uninteresting, but can be formed as `()` like in Haskell, and typed like `1` or (or `⊤`).
- To construct terms using `cata` or `case`, use _space_ to apply arguments. for instance, `case s f c` applies `s` to `case`, which then applies `f` to the `case s` and `g` to `case s f` etc...
- Inductive types are formed by prefixing a term with `in` and a type with `M` (or `μ`). Concretely, an inductive term looks like `in x:t` where `x` is the term and `t` is its type. Similarly, an inductive type looks like `μ(T)` where `T` is the type we insist is inductive. For example, peano nats have type `1+ μ(1+X)` where `1` is zero, and `X` is a μ variable capturing the subterm of type `1+μ(1+X)` (also a nat). The example term zero captures this idea above. See the semantics for a description of what these constructions mean.
- I should note that the parser is pretty verbose with it's types, a nice challenge would be to implement a backtracking parser as a means to reduce the number of necessary type annotations (in say the zero example above).
- To quit use `Ctrl+C` or whatever your machine uses to interrupt computations.

## Semantics

The semantics implements full beta-reduction on terms and alpha-equivalence as the `Eq` instance of `CataTerm`. The semantics are the same as the STLC but with additional rules for `In`, `Cata`, and all the machinery for products, sums and units. We reformulate the semantics as [typing judgements](https://existentialtype.wordpress.com/2011/03/27/the-holy-trinity/):

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

with the obvious rules for reduction inside each term that has a subterm. Finally, we have rules for `μ` type introduction, elimination and, reduction. Note how this forms a [fold](https://en.wikipedia.org/wiki/Fold_(higher-order_function)):

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{\Gamma\vdash&space;t&space;:&space;F\,(\mu&space;F)}{\Gamma\vdash&space;in\,&space;t&space;:&space;\mu\,F}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{\Gamma\vdash&space;t&space;:&space;F\,(\mu&space;F)}{\Gamma\vdash&space;in\,&space;t&space;:&space;\mu\,F}" title="\frac{\Gamma\vdash t : F\,(\mu F)}{\Gamma\vdash in\, t : \mu\,F}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{\Gamma\vdash&space;f&space;:&space;F\,X\rightarrow&space;X}{\Gamma\vdash\,cata\,f:\mu&space;F&space;\rightarrow&space;X}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{\Gamma\vdash&space;f&space;:&space;F\,X\rightarrow&space;X}{\Gamma\vdash\,cata\,f:\mu&space;F&space;\rightarrow&space;X}" title="\frac{\Gamma\vdash f : F\,X\rightarrow X}{\Gamma\vdash\,cata\,f:\mu F \rightarrow X}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=cata\,f\,(in\,t)&space;\rightsquigarrow\,f\,(F\,(cata\,f)\,&space;t)" target="_blank"><img src="https://latex.codecogs.com/gif.latex?cata\,f\,(in\,t)&space;\rightsquigarrow\,f\,(F\,(cata\,f)\,&space;t)" title="cata\,f\,(in\,t) \rightsquigarrow\,f\,(F\,(cata\,f)\, t)" /></a>

with reduction inside terms like above.

- The rules for variables, abstraction and application follow directly from STLC; the rules for products, sums, and units are largely standard in the literature. μ types in the [literature](https://www.cis.upenn.edu/~bcpierce/tapl/) are typically written as `μX.T` where T is a type that contains X. Since we only use one μ variable, we just write `T` (submit a PR with the more general form!)
- We adopt the categorical notation of a [functor](https://en.wikipedia.org/wiki/Functor), which expresses a mapping from terms to terms and types to types. For example, given the functor type `F X = 1 + X`, and type `Y = B`, then `F Y = 1 + Y = 1 + B`. similarly if we had a term `inl _ : F _`, then `inl b: F B` for some `b:B`. This idea translates directly into Cata, however we don't do type-level application, and so you must apply these types yourself before using them. Concretely, you might provide `F Y` as the type `1 + B` in Cata.
- We can construct functors using `+`, `*`, `->`. A functor is _strictly positive_ if it contains only constant variables `A,B,C,...`, μ variable `X`, `+`, `*`, and `->` such that `X` only occurs on the right of arrows. For every _s.p._ functor F, we can define an inductive type `μF`. A strictly positive check has not been implemented yet, behaviour for non-s.p functors is thus undefined (submit a PR, as it's an easy fix). 
- For instance, Peano numbers have the shape `F X = 1 + X` where 1 is zero and X is the inductive case (like _succ n_). An F-shaped term could be `inl ():1+X` and hence an inductive nat is `in (inl ():1+X): F (μF)`, however we must now unify `1+X` and `F (μF)`. Here `X` must be `μF`, where `F X = 1 + X` and so the final type is `1+μ(1+X)`, where zero is `in (inl ():1+μ(1+X)):1+μ(1+X)` in Cata. Unification is not implemented, so you must derive and input this type yourself (or submit a PR!).
- We can implement functions on functors too, supposing `F X = 1 + X`, then a function `F X -> X` might [reduce](https://www.schoolofhaskell.com/user/bartosz/understanding-algebras) F-shaped terms into a simpler form `X`. For instance, `\0:1+X.case 0 (\1:1.()) (\2:X.())` reduces terms of type `1+X` to `()`; in languages with useful types (like booleans), we might use this to implement more powerful constructs (like conditionals). 
- We can extend the notation of typed reduction by _lifting_ it to the inductive level, that is given `f:F X -> X`, we get `cata f:μF X -> X`. This forms the elimination rule for inductive types and is known as the _catamorphism_ of F.
- We combine our knowledge of catamorphisms and inductive types and get folds (finally!). Intuitively this can be understood as a way to de-construct inductive types into simpler ones (e.g. summing up a list of numbers). The reduction rule states that applying a catamorphism `cata f` to and inductive term `in t` will first lift the cataterm to a term `F (cata f)` of type `F (μF) -> F X` which is then applied to `t:F (μF)` so that `F (cata f) t` has type `F X`. We apply the result to `f` to get a term of type `X` which is the simplest form. The implementation of `F` is handled for you, run the reduction tracer to see what it does!
- This implementation follows a [small-step](https://cs.stackexchange.com/questions/43294/difference-between-small-and-big-step-operational-semantics) operational semantics and Berendregt's [variable convention](https://cs.stackexchange.com/questions/69323/barendregts-variable-convention-what-does-it-mean) (see `substitution` in Cata.hs). 
- Reductions include the one-step reduction (see `reduce1` in Cata.hs), the many-step reduction (see `reduce` in Cata.hs). 

## Other Implementation Details
- Cata.hs contains the Haskell implementation of the calculus, including substitution, reduction, and other useful things.
- Parser.hs contains the monadic parser combinators needed to parse input strings into typed-term ASTs for the calculus.
- Repl.hs contains a simple read-eval-print loop which hooks into main, and into the parser.
- Main.hs is needed for GHC to compile without any flags, it also invokes the repl.

For contributions, see the project to-do list or submit a PR with something you think it needs.


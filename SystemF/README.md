# System F
Haskell implementation on Jean Yves-Girard's System F. It has the features of STLC but with second-order abstraction over types, to enable parametric polymorphism.

This strongly normalising calculus was used to prove the consistency of mathematical analysis and later rediscovered by Reynolds as the paradigm supporting polymorphism.

## Prerequisites
You need [Haskell](https://www.haskell.org/), this compiles with GHC 8.2.2 at least (Stack resolver: lts-11.0).

## To Build & Run

To compile and run do:
`ghc -O2 -o sf Main`
then run `./sf`

Alternatively to use the GHCi Interpreter do:
`ghci Main`
then type `main`

In either case you get something like the following:
```
Welcome to the System F REPL
Type some terms or press Enter to leave.
>
```

Note: When run in GHCi, you don't have the luxury of escaped characters, backspace, delete etc...
Compile it using GHC if you need this.

## Examples 
Where you can then have some fun, try these examples:
- `LX.\x:X.x` where `L` stands for second-order abstraction.
- `(LX. \x:X.x) [PX.(X->X)->X->X]`
- `LX.\f:X->X.\x:X.x` this is _zero_.

- `λn:ΠX.(X->X)->X->X.ΛY.λf:Y->Y.λy:Y.f (n [Y] f y)` this is _succ_

Note: `Π` is the second-order product type, `Λ` is second-order abstraction and `[X]` is the type variable `X`. Alternatively typed as `P`, `L`, and `[X]` respectively.

The parser is smart enough to recognise λ, Π ,and Λ; so you can copy and paste from the output:
```
Welcome to the System F REPL
Type some terms or press Enter to leave.
>   LX.\x:X.x
=   ΛX.λx:X.x
>   ΛX.λx:X.x
=   ΛX.λx:X.x
```
`>` denotes the REPL waiting for input, `=` means no reductions occurred (it's the same term), `~>` denotes one reduction, and `~>*` denotes 0 or more reductions (although in practice this is 1 or more due to `=`).

There is also a reduction tracer, which should print each reduction step. prefix any string with `'` in order to see the reductions:
```
>   '(λn:ΠX.(X->X)->X->X.ΛY.λf:Y->Y.λy:Y.f (n [Y] f y)) (LX.\f:X->X.\x:X.x)
~>  ΛY.λf:Y->Y.λy:Y.f ((ΛX.λf:X->X.λx:X.x) [Y] f y)
~>  ΛY.λf:Y->Y.λy:Y.f ((λf:Y->Y.λx:Y.x) f y)
~>  ΛY.λf:Y->Y.λy:Y.f ((λx:Y.x) y)
~>  ΛY.λf:Y->Y.λy:Y.f y
```
Note: this is succ zero (or one) in Church Numeral format

There is also a typing mechanism, which should display the type or fail as usual.
```
>   t(λn:ΠX.(X->X)->X->X.ΛY.λf:Y->Y.λy:Y.f (n [Y] f y)) (LX.\f:X->X.\x:X.x)
ΠY.(Y->Y)->Y->Y
>   tLX.\x:X. x x
Cannot Type Term: LX.\x:X. x x
```
where `ΠY.(Y->Y)->Y->Y` is the System F type for Nats

Note: if you provide a non-normalizing term, the type checker will fail and reduction will not occur.

You can save terms for the life of the program with a `let` expression. Any time a saved variable appears in a term, it will be substituted for the saved term:
```
>   let zero = LX.\f:X->X.\x:X.x
Saved: ΛX.λf:X->X.λx:X.x
>   let succ = λn:ΠX.(X->X)->X->X.ΛY.λf:Y->Y.λy:Y.f (n [Y] f y)
Saved: λn:ΠX.(X->X)->X->X.ΛY.λf:Y->Y.λy:Y.f (n [Y] f y)
>   succ zero
~>* ΛY.λf:Y->Y.λy:Y.f y
```
Note: Consequently `let` and `=` are keywords, and so you cannot name variables with these. Additionally `L`, `[`, `]`, and `P` are keywords in System F.

## Syntax 

We base the language on the BNF for the typed calculus:

<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}&space;\mathbf{\tau}&&space;::=&space;&&space;\lambda&space;\mathbf{\upsilon}{\tt&space;:}\sigma&space;.&space;\mathbf{\tau}\\&space;&&space;|&space;&&space;\tau\,&space;\tau\\&space;&&space;|&space;&&space;\upsilon&space;\\&space;&&space;|&space;&&space;\Lambda&space;\mu.\tau\\&space;&|&&space;[\sigma]&space;&&\\&&\\&space;\upsilon&space;&&space;::=&space;&&space;\tt{a}&space;|&space;\tt{b}&space;|&space;\tt{c}&space;|&space;...&space;|&space;\tt{aa}&space;|&space;...&space;\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}&space;\mathbf{\tau}&&space;::=&space;&&space;\lambda&space;\mathbf{\upsilon}{\tt&space;:}\sigma&space;.&space;\mathbf{\tau}\\&space;&&space;|&space;&&space;\tau\,&space;\tau\\&space;&&space;|&space;&&space;\upsilon&space;\\&space;&&space;|&space;&&space;\Lambda&space;\mu.\tau\\&space;&|&&space;[\sigma]&space;&&\\&&\\&space;\upsilon&space;&&space;::=&space;&&space;\tt{a}&space;|&space;\tt{b}&space;|&space;\tt{c}&space;|&space;...&space;|&space;\tt{aa}&space;|&space;...&space;\end{matrix}" title="\begin{matrix} \mathbf{\tau}& ::= & \lambda \mathbf{\upsilon}{\tt :}\sigma . \mathbf{\tau}\\ & | & \tau\, \tau\\ & | & \upsilon \\ & | & \Lambda \mu.\tau\\ &|& [\sigma] &&\\&&\\ \upsilon & ::= & \tt{a} | \tt{b} | \tt{c} | ... | \tt{aa} | ... \end{matrix}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}&space;\sigma&space;&&space;::=&space;&&space;{\tt&space;\mu&space;}\\&space;&&space;|&space;&&space;\sigma&space;\rightarrow&space;\sigma\\&space;&|&&space;\Pi&space;\mu&space;.&space;\sigma\\&space;\\&space;\mu&space;&&space;::=&space;&&space;\tt{A}&space;|&space;\tt{B}&space;|&space;\tt{C}&space;|&space;...&space;|&space;\tt{AA}&space;|&space;...&space;\\&space;\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}&space;\sigma&space;&&space;::=&space;&&space;{\tt&space;\mu&space;}\\&space;&&space;|&space;&&space;\sigma&space;\rightarrow&space;\sigma\\&space;&|&&space;\Pi&space;\mu&space;.&space;\sigma\\&space;\\&space;\mu&space;&&space;::=&space;&&space;\tt{A}&space;|&space;\tt{B}&space;|&space;\tt{C}&space;|&space;...&space;|&space;\tt{AA}&space;|&space;...&space;\\&space;\end{matrix}" title="\begin{matrix} \sigma & ::= & {\tt \mu }\\ & | & \sigma \rightarrow \sigma\\ &|& \Pi \mu . \sigma\\ \\ \mu & ::= & \tt{A} | \tt{B} | \tt{C} | ... | \tt{AA} | ... \\ \end{matrix}" /></a>

However we adopt standard bracketing conventions to eliminate ambiguity in the parser. Concretely, the parser implements the non-ambiguous grammar for terms as follows:

<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}&space;&&\\&space;\mathbf{\tau}&&space;::=&space;&&space;\lambda&space;\mathbf{\upsilon}\tt{:}\sigma&space;.&space;\mathbf{\tau}\\&space;&|&&space;\Lambda\mu.\tau&space;\\&space;&&space;|&space;&&space;\alpha\\&space;&&\\&space;\alpha&space;&&space;::=&space;&&space;\beta&space;\\&space;&|&space;&\mathbf{\alpha\,&space;\tt{space}\,&space;\beta}&space;\\&space;&|&&space;\[&space;\sigma\]&space;\\&space;&&\\&space;\beta&space;&&space;::=&space;&&space;\tt{(}\tau&space;\tt{)}\\&space;&|&&space;\upsilon&space;\\&space;&&\\&space;\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}&space;&&\\&space;\mathbf{\tau}&&space;::=&space;&&space;\lambda&space;\mathbf{\upsilon}\tt{:}\sigma&space;.&space;\mathbf{\tau}\\&space;&|&&space;\Lambda\mu.\tau&space;\\&space;&&space;|&space;&&space;\alpha\\&space;&&\\&space;\alpha&space;&&space;::=&space;&&space;\beta&space;\\&space;&|&space;&\mathbf{\alpha\,&space;\tt{space}\,&space;\beta}&space;\\&space;&|&&space;\[&space;\sigma\]&space;\\&space;&&\\&space;\beta&space;&&space;::=&space;&&space;\tt{(}\tau&space;\tt{)}\\&space;&|&&space;\upsilon&space;\\&space;&&\\&space;\end{matrix}" title="\begin{matrix} &&\\ \mathbf{\tau}& ::= & \lambda \mathbf{\upsilon}\tt{:}\sigma . \mathbf{\tau}\\ &|& \Lambda\mu.\tau \\ & | & \alpha\\ &&\\ \alpha & ::= & \beta \\ &| &\mathbf{\alpha\, \tt{space}\, \beta} \\ &|& \[ \sigma\] \\ &&\\ \beta & ::= & \tt{(}\tau \tt{)}\\ &|& \upsilon \\ &&\\ \end{matrix}" /></a>

and types:

<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}&space;\sigma&space;&&space;::=&space;&&space;\gamma&space;\\&space;&&space;|&space;&&space;\gamma&space;\tt{\rightarrow}&space;\sigma&space;\\&space;&|&&space;\Pi\mu.\sigma\\&space;&&\\&space;\gamma&space;&&space;::=&&space;\tt{(}&space;\sigma&space;\tt{)}\\&space;&&space;|&\,&space;\mu&space;\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}&space;\sigma&space;&&space;::=&space;&&space;\gamma&space;\\&space;&&space;|&space;&&space;\gamma&space;\tt{\rightarrow}&space;\sigma&space;\\&space;&|&&space;\Pi\mu.\sigma\\&space;&&\\&space;\gamma&space;&&space;::=&&space;\tt{(}&space;\sigma&space;\tt{)}\\&space;&&space;|&\,&space;\mu&space;\end{matrix}" title="\begin{matrix} \sigma & ::= & \gamma \\ & | & \gamma \tt{\rightarrow} \sigma \\ &|& \Pi\mu.\sigma\\ &&\\ \gamma & ::=& \tt{(} \sigma \tt{)}\\ & |&\, \mu \end{matrix}" /></a>

with term variables:

<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}&space;\upsilon&space;&&space;::=&space;&&space;\tt{a}&space;|&space;\tt{b}&space;|&space;\tt{c}&space;|&space;...&space;|&space;\tt{aa}&space;|&space;...&space;\\\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}&space;\upsilon&space;&&space;::=&space;&&space;\tt{a}&space;|&space;\tt{b}&space;|&space;\tt{c}&space;|&space;...&space;|&space;\tt{aa}&space;|&space;...&space;\\\end{matrix}" title="\begin{matrix} \upsilon & ::= & \tt{a} | \tt{b} | \tt{c} | ... | \tt{aa} | ... \\\end{matrix}" /></a>

and type variables:

<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}&space;\upsilon&space;&&space;::=&space;&&space;\tt{A}&space;|&space;\tt{B}&space;|&space;\tt{C}&space;|&space;...&space;|&space;\tt{AA}&space;|&space;...&space;\\\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}&space;\upsilon&space;&&space;::=&space;&&space;\tt{A}&space;|&space;\tt{B}&space;|&space;\tt{C}&space;|&space;...&space;|&space;\tt{AA}&space;|&space;...&space;\\\end{matrix}" title="\begin{matrix} \upsilon & ::= & \tt{A} | \tt{B} | \tt{C} | ... | \tt{AA} | ... \\\end{matrix}" /></a>

Some notes about the syntax:

- The above syntax only covers the core calculus, and not the repl extensions (such as let bindings above). The extensions are simply added on in the repl.
- Variables are strings (excluding numbers), as this is isomorphic to a whiteboard treatment and hence the most familiar.
- Types are uppercase strings for the same reasons. Type variables must be distinct from term variables.
- Types are either type variables, abstractions, or nested arrow types: `T -> T`. Arrows associate to the right so that `T -> T -> T` is the same as `T -> (T -> T)` but not `((T -> T) -> T)`. The product binds weaker than arrows, so `ΠX.X->X` is the same as `ΠX.(X->X)`. 
- Nested terms don't need brackets: `LX.LY.\x:X.\y:Y. y` unless enforcing application on the right. Whitespace does not matter `LX.\x:X.          x` unless it is between application where you need at least one space.
- To quit use `Ctrl+C` or whatever your machine uses to interrupt computations.

## Semantics

The semantics implements beta-reduction on terms and alpha-equivalence as the `Eq` instance of `SFTerm`. The semantics are the same as STLC with the addition of second-order abstraction over types. We reformulate the semantics as [typing judgements](https://existentialtype.wordpress.com/2011/03/27/the-holy-trinity/):

for variables (adopted from STLC):

<a href="https://www.codecogs.com/eqnedit.php?latex=\overline{\Gamma&space;\vdash&space;x:T},\quad&space;\mbox{(if&space;$x:T&space;\in&space;\Gamma$)}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\overline{\Gamma&space;\vdash&space;x:T},\quad&space;\mbox{(if&space;$x:T&space;\in&space;\Gamma$)}" title="\overline{\Gamma \vdash x:T},\quad \mbox{(if $x:T \in \Gamma$)}" /></a>

for abstractions (adopted from STLC):

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{\Gamma&space;,x:A&space;\vdash&space;t:B}{\Gamma&space;\vdash&space;(\lambda&space;x&space;:&space;A.&space;t)&space;:&space;B&space;}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{\Gamma&space;,x:A&space;\vdash&space;t:B}{\Gamma&space;\vdash&space;(\lambda&space;x&space;:&space;A.&space;t)&space;:&space;B&space;}" title="\frac{\Gamma ,x:A \vdash t:B}{\Gamma \vdash (\lambda x : A. t) : B }" /></a>

and application (adopted from STLC):

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{\Gamma&space;\vdash&space;f&space;:&space;A&space;\Rightarrow&space;B\quad&space;\Gamma&space;\vdash&space;x&space;:&space;A}{\Gamma&space;\vdash&space;(f&space;x)&space;:&space;B}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{\Gamma&space;\vdash&space;f&space;:&space;A&space;\Rightarrow&space;B\quad&space;\Gamma&space;\vdash&space;x&space;:&space;A}{\Gamma&space;\vdash&space;(f&space;x)&space;:&space;B}" title="\frac{\Gamma \vdash f : A \Rightarrow B\quad \Gamma \vdash x : A}{\Gamma \vdash (f x) : B}" /></a>

and the reduction relation (adopted from STLC):

<a href="https://www.codecogs.com/eqnedit.php?latex=(\lambda&space;x&space;:&space;T&space;.&space;M)N&space;\rightsquigarrow&space;M&space;[x&space;:=&space;N]" target="_blank"><img src="https://latex.codecogs.com/gif.latex?(\lambda&space;x&space;:&space;T&space;.&space;M)N&space;\rightsquigarrow&space;M&space;[x&space;:=&space;N]" title="(\lambda x : T . M)N \rightsquigarrow M [x := N]" /></a>

and special introduction, elimination, and reduction rules for types:

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{\Gamma\,,&space;X\,&space;Type&space;\vdash&space;t&space;:&space;T}{\Gamma&space;\vdash&space;(\Lambda&space;X.t):\Pi&space;X.T}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{\Gamma\,,&space;X\,&space;Type&space;\vdash&space;t&space;:&space;T}{\Gamma&space;\vdash&space;(\Lambda&space;X.t):\Pi&space;X.T}" title="\frac{\Gamma\,, X\, Type \vdash t : T}{\Gamma \vdash (\Lambda X.t):\Pi X.T}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{\Gamma&space;\vdash&space;f&space;:&space;\Pi&space;X.T}{\Gamma&space;\vdash&space;(f\,&space;A):T[X&space;:=&space;A]}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{\Gamma&space;\vdash&space;f&space;:&space;\Pi&space;X.T}{\Gamma&space;\vdash&space;(f\,&space;A):T[X&space;:=&space;A]}" title="\frac{\Gamma \vdash f : \Pi X.T}{\Gamma \vdash (f\, A):T[X := A]}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=(\Lambda&space;X.t)\,A&space;\rightsquigarrow&space;t[X:=A]" target="_blank"><img src="https://latex.codecogs.com/gif.latex?(\Lambda&space;X.t)\,A&space;\rightsquigarrow&space;t[X:=A]" title="(\Lambda X.t)\,A \rightsquigarrow t[X:=A]" /></a>

- This means the typing context now also contains types, and types occur in terms. The phrase `X Type` means X is a type. We do not implement Agda style type hierarchies here.
- This implementation follows a [small-step](https://cs.stackexchange.com/questions/43294/difference-between-small-and-big-step-operational-semantics) operational semantics and Berendregt's [variable convention](https://cs.stackexchange.com/questions/69323/barendregts-variable-convention-what-does-it-mean) (see `substitution` in SystemF.hs). The variable convention is adopted for both types and terms.
- Reductions include the one-step reduction (see `reduce1` in SystemF.hs), the many-step reduction (see `reduce` in SystemF.hs). 

## Other Implementation Details
- SystemF.hs contains the Haskell implementation of the calculus, including substitution, reduction, and other useful things.
- Parser.hs contains the monadic parser combinators needed to parse input strings into typed-term ASTs for the calculus.
- Repl.hs contains a simple read-eval-print loop which hooks into main, and into the parser.
- Main.hs is needed for GHC to compile without any flags, it also invokes the repl.

For contributions, see the project to-do list or submit a PR with something you think it needs.

Work initially documented [here](https://gist.github.com/lukeg101/f1c13024cf9ccbeaff3c3553baca037f).



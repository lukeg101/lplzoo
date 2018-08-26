# System F Omega
TODO

## Prerequisites
You need [Haskell](https://www.haskell.org/), this compiles with GHC 8.2.2 at least (Stack resolver: lts-11.0).

## To Build & Run

To compile and run do:
`ghc -O2 -o fomega Main`
then run `./fomega`

Alternatively to use the GHCi Interpreter do:
`ghci Main`
then type `main`

In either case you get something like the following:
```
Welcome to the FOmega REPL
Type some terms or press Enter to leave.
>
```

Note: When run in GHCi, you don't have the luxury of escaped characters, backspace, delete etc...
Compile it using GHC if you need this.

## Examples 
Where you can then have some fun, try these examples:
- `LX::*.\x:X.x` where `L` stands for second-order product.
- `(LX::*. \x:X.x) [PX::*.(X->X)->X->X]` the identity function for natural numbers. 
- `LX::*.\f:X->X.\x:X.x` this is _zero_, Church style.
- `z` This is zero, in Peano style
- `s (s (s z))` This is 3 in Peano style.
- `\x:Nat. x` This is the identity function on Nats. 
- `\x:(\A::*.A) Nat.x` This is the identity function except with type-level abstraction and application. The type-level abstraction accepts proper types (kinded `*`) such as Nat, and returns a type (of kind `*`) such that the resultant kind is `* => *`. The type-level application applies Nat to the abstraction and returns Nat.
- `λn:ΠX::*.(X->X)->X->X.ΛY::*.λf:Y->Y.λy:Y.f (n [Y] f y)` this is _succ_, church-style

Note: `Π` is the second-order product type, `Λ` is second-order abstraction (term-level) and `[X]` is the type variable `X`. Alternatively typed as `P`, `L`, and `[X]` respectively.

The parser is smart enough to recognise λ, Π ,and Λ; so you can copy and paste from the output:
```
Welcome to the System F REPL
Type some terms or press Enter to leave.
>   LX::*.\x:X.x
=   ΛX::*.λx:X.x
>   ΛX::*.λx:X.x
=   ΛX::*.λx:X.x
```

`>` denotes the REPL waiting for input, `=` means no reductions occurred (it's the same term), `~>` denotes one reduction, and `~>*` denotes 0 or more reductions (although in practice this is 1 or more due to `=`).

There is also a reduction tracer, which should print each reduction step. prefix any string with `'` in order to see the reductions:
```
>   '(λn:ΠX::*.(X->X)->X->X.ΛY::*.λf:Y->Y.λy:Y.f (n [Y] f y)) (LX::*.\f:X->X.\x:X.x)
~>  ΛY::*.λf:Y->Y.λy:Y.f ((ΛX::*.λf:X->X.λx:X.x) [Y] f y)
~>  ΛY::*.λf:Y->Y.λy:Y.f ((λf:Y->Y.λx:Y.x) f y)
~>  ΛY::*.λf:Y->Y.λy:Y.f ((λx:Y.x) y)
~>  ΛY::*.λf:Y->Y.λy:Y.f y
```
Note: this is succ zero (or one) in Church Numeral format

Reduction occurs at both the term level and type level (inside term-level abstractions as this is where the types are). Here's a function that reduces only at the type level:
```
>   '\x:(\PAIR::(*=>*=>*)=>*.PAIR (\A::*.\B::*.A)) ((\A::*.\B::*.\PAIR::*=>*=>*.PAIR A B) Nat Nat).x
~>  λx:(λA::*.λB::*.λPAIR::*=>*=>*.PAIR A B) Nat Nat (λA::*.λB::*.A).x
~>  λx:(λB::*.λPAIR::*=>*=>*.PAIR Nat B) Nat (λA::*.λB::*.A).x
~>  λx:(λPAIR::*=>*=>*.PAIR Nat Nat) (λA::*.λB::*.A).x
~>  λx:(λA::*.λB::*.A) Nat Nat.x
~>  λx:(λB::*.Nat) Nat.x
~>  λx:Nat.x
```

There is also a typing mechanism, which should display the type or fail as usual.
```
>   tLX::*.\x:X. x x
Cannot Type Term: LX::*.\x:X. x x
>   t\x:(\A::*.A) Nat.x
(λA::*.A) Nat->(λA::*.A) Nat
>   t\x:(\A::*.A).x
Cannot Type Term: \x:(\A::*.A).x
```
Note: The above is untypeable as `λA::*.A` is a type operator, such that it takes a type and returns a type (not a term). See the semantics for details on why.

`ΠY::*.(Y->Y)->Y->Y` is the type for Church-style Nats encoded using the second-order product. 

Note: if you provide a non-normalizing term, the type checker will fail and reduction will not occur.

You can save terms for the life of the program with a `let` expression. Any time a saved variable appears in a term, it will be substituted for the saved term:
```
>   let zero = LX::*.\f:X->X.\x:X.x
Saved term: ΛX::*.λf:X->X.λx:X.x
>   let succ = λn:ΠX::*.(X->X)->X->X.ΛY::*.λf:Y->Y.λy:Y.f (n [Y] f y)
Saved term: λn:ΠX::*.(X->X)->X->X.ΛY::*.λf:Y->Y.λy:Y.f (n [Y] f y)
>   succ zero
~>* ΛY::*.λf:Y->Y.λy:Y.f y
```

Note: Consequently `let` and `=` are keywords, and so you cannot name variables with these. Additionally `L`, `[`, `]`, and `P` are keywords in F Omega.

Note: We include introduction rules for Nats to show how kinding works for proper types. We do not include Nat eliminators however as the focus is on type operators. Use the calculus with this in mind, like the next example. Additionally we can encode nats easily, church-style with product types 

Additionally we have type level `lett` statements that allow you to define and use types:
```
>   let zero = LX::*.\f:X->X.\x:X.x
Saved term: ΛX::*.λf:X->X.λx:X.x
>   lett NAT = PX::*.(X->X)->X->X
Saved type: ΠX::*.(X->X)->X->X
>   let succ = \n:NAT.LX::*.\f:X->X.\x:X.f (n [X] f x)
Saved term: λn:ΠX::*.(X->X)->X->X.ΛX::*.λf:X->X.λx:X.f (n [X] f x)
>   succ zero
~>* ΛX::*.λf:X->X.λx:X.f x
```
and 
```
>   lett PAIR = \A::*.\B::*.\PAIR::*=>*=>*.PAIR A B
Saved type: λA::*.λB::*.λPAIR::*=>*=>*.PAIR A B
>   lett FST = \PAIR::(*=>*=>*)=>*.PAIR (\A::*.\B::*.A)
Saved type: λPAIR::(*=>*=>*)=>*.PAIR (λA::*.λB::*.A)
>   \x:FST (PAIR Nat Nat).x
~>* λx:Nat.x
```

This makes it easier to define both terms and types, but does not allow type level application (See Omega). `lett` is also a keyword.

## Syntax 

We base the language on the BNF for the typed calculus:

<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}&space;\mathbf{\tau}&&space;::=&space;&&space;\lambda&space;\mathbf{\upsilon}{\tt&space;:}\sigma&space;.&space;\mathbf{\tau}\\&space;&&space;|&space;&&space;\tau\,&space;\tau\\&space;&&space;|&space;&&space;\upsilon&space;\\&space;&&space;|&space;&&space;\Lambda&space;\theta::\kappa.\tau\\&space;&|&&space;[\sigma]&space;&&\\&|&&space;{\tt&space;z}&space;\\&|&{\tt&space;s}\,&space;\tau\\&&\\&space;\upsilon&space;&&space;::=&space;&&space;\tt{a}&space;|&space;\tt{b}&space;|&space;\tt{c}&space;|&space;...&space;|&space;\tt{aa}&space;|&space;...&space;\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}&space;\mathbf{\tau}&&space;::=&space;&&space;\lambda&space;\mathbf{\upsilon}{\tt&space;:}\sigma&space;.&space;\mathbf{\tau}\\&space;&&space;|&space;&&space;\tau\,&space;\tau\\&space;&&space;|&space;&&space;\upsilon&space;\\&space;&&space;|&space;&&space;\Lambda&space;\theta::\kappa.\tau\\&space;&|&&space;[\sigma]&space;&&\\&|&&space;{\tt&space;z}&space;\\&|&{\tt&space;s}\,&space;\tau\\&&\\&space;\upsilon&space;&&space;::=&space;&&space;\tt{a}&space;|&space;\tt{b}&space;|&space;\tt{c}&space;|&space;...&space;|&space;\tt{aa}&space;|&space;...&space;\end{matrix}" title="\begin{matrix} \mathbf{\tau}& ::= & \lambda \mathbf{\upsilon}{\tt :}\sigma . \mathbf{\tau}\\ & | & \tau\, \tau\\ & | & \upsilon \\ & | & \Lambda \theta::\kappa.\tau\\ &|& [\sigma] &&\\&|& {\tt z} \\&|&{\tt s}\, \tau\\&&\\ \upsilon & ::= & \tt{a} | \tt{b} | \tt{c} | ... | \tt{aa} | ... \end{matrix}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}&space;\sigma&space;&&space;::=&space;&&space;\lambda&space;\mathbf{\theta&space;}\tt{::}\kappa.\sigma\\&|&&space;\sigma\,{\tt&space;space}\,\sigma\\&space;&|&&space;\theta\\&space;&&space;|&space;&&space;\sigma&space;\rightarrow&space;\sigma&space;\\&space;&|&&space;Nat\\&space;&|&&space;\Pi&space;\theta::\kappa&space;.&space;\sigma\\&space;\\&space;\theta&space;&&space;::=&space;&&space;\tt{A}&space;|&space;\tt{B}&space;|&space;\tt{C}&space;|&space;...&space;|&space;\tt{AA}&space;|&space;...&space;\\&space;\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}&space;\sigma&space;&&space;::=&space;&&space;\lambda&space;\mathbf{\theta&space;}\tt{::}\kappa.\sigma\\&|&&space;\sigma\,{\tt&space;space}\,\sigma\\&space;&|&&space;\theta\\&space;&&space;|&space;&&space;\sigma&space;\rightarrow&space;\sigma&space;\\&space;&|&&space;Nat\\&space;&|&&space;\Pi&space;\theta::\kappa&space;.&space;\sigma\\&space;\\&space;\theta&space;&&space;::=&space;&&space;\tt{A}&space;|&space;\tt{B}&space;|&space;\tt{C}&space;|&space;...&space;|&space;\tt{AA}&space;|&space;...&space;\\&space;\end{matrix}" title="\begin{matrix} \sigma & ::= & \lambda \mathbf{\theta }\tt{::}\kappa.\sigma\\&|& \sigma\,{\tt space}\,\sigma\\ &|& \theta\\ & | & \sigma \rightarrow \sigma \\ &|& Nat\\ &|& \Pi \theta::\kappa . \sigma\\ \\ \theta & ::= & \tt{A} | \tt{B} | \tt{C} | ... | \tt{AA} | ... \\ \end{matrix}" /></a>

However we adopt standard bracketing conventions to eliminate ambiguity in the parser. Concretely, the parser implements the non-ambiguous grammar for terms as follows:

<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}&space;&&\\&space;\mathbf{\tau}&&space;::=&space;&&space;\lambda&space;\mathbf{\upsilon}\tt{:}\sigma&space;.&space;\mathbf{\tau}\\&space;&&space;|&space;&&space;\alpha\\&space;&&\\&space;\alpha&space;&&space;::=&space;&&space;\Lambda\theta::\kappa.\tau\\&space;&|&&space;\beta&space;\\\\&space;\beta&space;&::=&space;&\mathbf{\beta\,&space;\tt{space}\,&space;\phi}&space;\\&space;&|&&space;\phi\\\\&space;\phi&space;&&space;::=&space;&&space;\tt{(}\tau&space;\tt{)}\\&space;&|&&space;\upsilon&space;\\&space;&|&&space;{\tt&space;z}\\&space;&|&&space;{\tt&space;s}&space;\\&space;&|&&space;[\sigma]\\&space;\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}&space;&&\\&space;\mathbf{\tau}&&space;::=&space;&&space;\lambda&space;\mathbf{\upsilon}\tt{:}\sigma&space;.&space;\mathbf{\tau}\\&space;&&space;|&space;&&space;\alpha\\&space;&&\\&space;\alpha&space;&&space;::=&space;&&space;\Lambda\theta::\kappa.\tau\\&space;&|&&space;\beta&space;\\\\&space;\beta&space;&::=&space;&\mathbf{\beta\,&space;\tt{space}\,&space;\phi}&space;\\&space;&|&&space;\phi\\\\&space;\phi&space;&&space;::=&space;&&space;\tt{(}\tau&space;\tt{)}\\&space;&|&&space;\upsilon&space;\\&space;&|&&space;{\tt&space;z}\\&space;&|&&space;{\tt&space;s}&space;\\&space;&|&&space;[\sigma]\\&space;\end{matrix}" title="\begin{matrix} &&\\ \mathbf{\tau}& ::= & \lambda \mathbf{\upsilon}\tt{:}\sigma . \mathbf{\tau}\\ & | & \alpha\\ &&\\ \alpha & ::= & \Lambda\theta::\kappa.\tau\\ &|& \beta \\\\ \beta &::= &\mathbf{\beta\, \tt{space}\, \phi} \\ &|& \phi\\\\ \phi & ::= & \tt{(}\tau \tt{)}\\ &|& \upsilon \\ &|& {\tt z}\\ &|& {\tt s} \\ &|& [\sigma]\\ \end{matrix}" /></a>

and types:

<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}&space;&&\\&space;\mathbf{\sigma}&&space;::=&space;&&space;\lambda&space;\mathbf{\theta}\tt{::}\kappa&space;.&space;\mathbf{\sigma}\\&space;&&space;|&space;&&space;\eta\\&space;&&\\&space;\eta&space;&&space;::=&space;&&space;\Pi\theta::\kappa.\eta\\&space;&|&\delta&space;\\&space;&&\\&space;\delta&space;&&space;::=&space;&&space;\delta&space;\rightarrow&space;\epsilon\\&space;&|&&space;\epsilon\\\\&space;\epsilon&space;&&space;::=&space;&&space;\mathbf{\epsilon\,&space;\tt{space}\,\rho&space;}&space;\\&space;&|&&space;\rho\\\\&space;\rho&space;&::=&\tt{(}\sigma&space;\tt{)}\\&space;&|&&space;\theta&space;\\&|&&space;Nat&space;\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}&space;&&\\&space;\mathbf{\sigma}&&space;::=&space;&&space;\lambda&space;\mathbf{\theta}\tt{::}\kappa&space;.&space;\mathbf{\sigma}\\&space;&&space;|&space;&&space;\eta\\&space;&&\\&space;\eta&space;&&space;::=&space;&&space;\Pi\theta::\kappa.\eta\\&space;&|&\delta&space;\\&space;&&\\&space;\delta&space;&&space;::=&space;&&space;\delta&space;\rightarrow&space;\epsilon\\&space;&|&&space;\epsilon\\\\&space;\epsilon&space;&&space;::=&space;&&space;\mathbf{\epsilon\,&space;\tt{space}\,\rho&space;}&space;\\&space;&|&&space;\rho\\\\&space;\rho&space;&::=&\tt{(}\sigma&space;\tt{)}\\&space;&|&&space;\theta&space;\\&|&&space;Nat&space;\end{matrix}" title="\begin{matrix} &&\\ \mathbf{\sigma}& ::= & \lambda \mathbf{\theta}\tt{::}\kappa . \mathbf{\sigma}\\ & | & \eta\\ &&\\ \eta & ::= & \Pi\theta::\kappa.\eta\\ &|&\delta \\ &&\\ \delta & ::= & \delta \rightarrow \epsilon\\ &|& \epsilon\\\\ \epsilon & ::= & \mathbf{\epsilon\, \tt{space}\,\rho } \\ &|& \rho\\\\ \rho &::=&\tt{(}\sigma \tt{)}\\ &|& \theta \\&|& Nat \end{matrix}" /></a>

for kinds (equivalent to the parser for `O` and `T->T` in STLC):

<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}\kappa&space;&&space;::=&space;&&space;\gamma&space;\\&space;&&space;|&space;&&space;\gamma\,&space;\tt{\Rightarrow}\&space;\kappa&space;\\&space;&&\\&space;\gamma&space;&&space;::=&&space;\tt{(}&space;\kappa&space;\tt{)}\,&space;|\,&space;\tt{*}&space;\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}\kappa&space;&&space;::=&space;&&space;\gamma&space;\\&space;&&space;|&space;&&space;\gamma\,&space;\tt{\Rightarrow}\&space;\kappa&space;\\&space;&&\\&space;\gamma&space;&&space;::=&&space;\tt{(}&space;\kappa&space;\tt{)}\,&space;|\,&space;\tt{*}&space;\end{matrix}" title="\begin{matrix}\kappa & ::= & \gamma \\ & | & \gamma\, \tt{\Rightarrow}\ \kappa \\ &&\\ \gamma & ::=& \tt{(} \kappa \tt{)}\, |\, \tt{*} \end{matrix}" /></a>

with term variables:

<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}&space;\upsilon&space;&&space;::=&space;&&space;\tt{a}&space;|&space;\tt{b}&space;|&space;\tt{c}&space;|&space;...&space;|&space;\tt{aa}&space;|&space;...&space;\\\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}&space;\upsilon&space;&&space;::=&space;&&space;\tt{a}&space;|&space;\tt{b}&space;|&space;\tt{c}&space;|&space;...&space;|&space;\tt{aa}&space;|&space;...&space;\\\end{matrix}" title="\begin{matrix} \upsilon & ::= & \tt{a} | \tt{b} | \tt{c} | ... | \tt{aa} | ... \\\end{matrix}" /></a>

and type variables:

<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}&space;\theta&space;&&space;::=&space;&&space;\tt{A}&space;|&space;\tt{B}&space;|&space;\tt{C}&space;|&space;...&space;|&space;\tt{AA}&space;|&space;...&space;\\\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}&space;\theta&space;&&space;::=&space;&&space;\tt{A}&space;|&space;\tt{B}&space;|&space;\tt{C}&space;|&space;...&space;|&space;\tt{AA}&space;|&space;...&space;\\\end{matrix}" title="\begin{matrix} \theta & ::= & \tt{A} | \tt{B} | \tt{C} | ... | \tt{AA} | ... \\\end{matrix}" /></a>

Some notes about the syntax:

- The above syntax only covers the core calculus, and not the repl extensions (such as let bindings above). The extensions are simply added on in the repl.
- Term and type variables are strings (excluding numbers), as this is isomorphic to a whiteboard treatment and hence the most familiar. Term variables are lower case whereas type variables are upper case.
- Types are variables `A,B,C,...`, type operators/abstractions `\X::K.T`, applications `A B`, arrows `A -> B`, product types `ΠX::*.(X->X)->X->X` or Nats. 
- Omega formally introduces type variables `A, B, C...` which until now are used informally for convenience. STLC does not have variables for this reason. Some treatments of STLC range over base types `A,B,C,...` which implicitly assumes these are proper types. 
- Type arrows associate to the right so that `X -> Y -> Z` is the same as `X -> (Y -> Z)` but not `((X -> Y) -> Z)`. The same is true at the kind level `* => * => *` is the same as `* => (* => *)` but not `(* => *) => *`.
- The second-order product type binds weaker than arrows, so `ΠX::*.X->X` is the same as `ΠX::*.(X->X)`. 
- For both term and types, nested terms don't need brackets: `LX::*.LY::*.\x:X.\y:Y. y` unless enforcing application on the right. Whitespace does not matter `LX::*.\x:X.          x` unless it is between application where you need at least one space.
- To quit use `Ctrl+C` or whatever your machine uses to interrupt computations.

## Semantics

The semantics implements beta-reduction on terms and alpha-equivalence as the `Eq` instance of `FOTerm`. The semantics are the same as STLC with the addition of second-order abstraction over types, kinds, and a type operators. We reformulate the semantics as [typing judgements](https://existentialtype.wordpress.com/2011/03/27/the-holy-trinity/):
for term-level variables:

<a href="https://www.codecogs.com/eqnedit.php?latex=\overline{\Gamma&space;\vdash&space;x:T},\quad&space;\mbox{(if&space;$x:T&space;\in&space;\Gamma$)}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\overline{\Gamma&space;\vdash&space;x:T},\quad&space;\mbox{(if&space;$x:T&space;\in&space;\Gamma$)}" title="\overline{\Gamma \vdash x:T},\quad \mbox{(if $x:T \in \Gamma$)}" /></a>

for term-level abstractions, with the added constraint that the variable in the abstraction has a proper type (`*`):

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{\Gamma&space;\vdash&space;A&space;::&space;*&space;\quad&space;\Gamma&space;\vdash&space;f&space;:&space;A&space;\Rightarrow&space;B\quad&space;\Gamma&space;\vdash&space;x&space;:&space;A}{\Gamma&space;\vdash&space;(f&space;x)&space;:&space;B}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{\Gamma&space;\vdash&space;A&space;::&space;*&space;\quad&space;\Gamma&space;\vdash&space;f&space;:&space;A&space;\Rightarrow&space;B\quad&space;\Gamma&space;\vdash&space;x&space;:&space;A}{\Gamma&space;\vdash&space;(f&space;x)&space;:&space;B}" title="\frac{\Gamma \vdash A :: * \quad \Gamma \vdash f : A \Rightarrow B\quad \Gamma \vdash x : A}{\Gamma \vdash (f x) : B}" /></a>

and term-level application:

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{\Gamma&space;\vdash&space;f&space;:&space;A&space;\Rightarrow&space;B\quad&space;\Gamma&space;\vdash&space;x&space;:&space;A}{\Gamma&space;\vdash&space;(f&space;x)&space;:&space;B}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{\Gamma&space;\vdash&space;f&space;:&space;A&space;\Rightarrow&space;B\quad&space;\Gamma&space;\vdash&space;x&space;:&space;A}{\Gamma&space;\vdash&space;(f&space;x)&space;:&space;B}" title="\frac{\Gamma \vdash f : A \Rightarrow B\quad \Gamma \vdash x : A}{\Gamma \vdash (f x) : B}" /></a>

for zero and succ:

<a href="https://www.codecogs.com/eqnedit.php?latex=\overline{\Gamma&space;\vdash&space;z:Nat}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\overline{\Gamma&space;\vdash&space;z:Nat}" title="\overline{\Gamma \vdash z:Nat}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{\Gamma&space;\vdash&space;n&space;:&space;Nat}{\Gamma&space;\vdash&space;s\,&space;n&space;:&space;Nat}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{\Gamma&space;\vdash&space;n&space;:&space;Nat}{\Gamma&space;\vdash&space;s\,&space;n&space;:&space;Nat}" title="\frac{\Gamma \vdash n : Nat}{\Gamma \vdash s\, n : Nat}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{n&space;\rightsquigarrow&space;n'}{s\,&space;n&space;\rightsquigarrow&space;s\,n'}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{n&space;\rightsquigarrow&space;n'}{s\,&space;n&space;\rightsquigarrow&space;s\,n'}" title="\frac{n \rightsquigarrow n'}{s\, n \rightsquigarrow s\,n'}" /></a>

and the reduction relation adopted from the untyped theory (with types added in the abstraction):

<a href="https://www.codecogs.com/eqnedit.php?latex=(\lambda&space;x&space;:&space;T&space;.&space;M)N&space;\rightsquigarrow&space;M&space;[x&space;:=&space;N]" target="_blank"><img src="https://latex.codecogs.com/gif.latex?(\lambda&space;x&space;:&space;T&space;.&space;M)N&space;\rightsquigarrow&space;M&space;[x&space;:=&space;N]" title="(\lambda x : T . M)N \rightsquigarrow M [x := N]" /></a>

and special introduction, elimination, and reduction rules for types (now with kinds):

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{\Gamma\,,&space;X\,::K&space;\vdash&space;t&space;:&space;T}{\Gamma&space;\vdash&space;(\Lambda&space;X::K.t):\Pi&space;X::K.T}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{\Gamma\,,&space;X\,::K&space;\vdash&space;t&space;:&space;T}{\Gamma&space;\vdash&space;(\Lambda&space;X::K.t):\Pi&space;X::K.T}" title="\frac{\Gamma\,, X\,::K \vdash t : T}{\Gamma \vdash (\Lambda X::K.t):\Pi X::K.T}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{\Gamma&space;\vdash&space;f&space;:&space;\Pi&space;X::K.T\quad\Gamma\vdash&space;A::K}{\Gamma&space;\vdash&space;(f\,&space;[A]):T[X&space;:=&space;A]}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{\Gamma&space;\vdash&space;f&space;:&space;\Pi&space;X::K.T\quad\Gamma\vdash&space;A::K}{\Gamma&space;\vdash&space;(f\,&space;[A]):T[X&space;:=&space;A]}" title="\frac{\Gamma \vdash f : \Pi X::K.T\quad\Gamma\vdash A::K}{\Gamma \vdash (f\, [A]):T[X := A]}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=(\Lambda&space;X::K.t)\,A&space;\rightsquigarrow&space;t[X:=A]" target="_blank"><img src="https://latex.codecogs.com/gif.latex?(\Lambda&space;X::K.t)\,A&space;\rightsquigarrow&space;t[X:=A]" title="(\Lambda X::K.t)\,A \rightsquigarrow t[X:=A]" /></a>

- This means the typing context now also contains types, and types occur in terms. The phrase `X :: K` means X is has kind K. We do not implement Agda style type hierarchies here.
- There is the additional constraint that any term abstractions must take a term of proper type. This prevents nonsensical or partially-applied types terms such as `Pair Nat _`.
- This reflects the principle that any typeable term has a kindable type.
- The base calculus on its own does not have any proper types (such as Nat, Bool or Unit) and so it is unusable as no terms can be introduced. To demonstrate proper types, we add the proper type `Nat`, with constructors `z` and `s`.
- Additionally with second-order products it is possible to replicate Church-style Nats as demonstrated above.
- Type abstractions can be of any kind permitted by Omega, however the resultant type must be proper for it to be accepted by a term-level abstraction. This prevents nonsensical terms and types like `z (s z)` typed `Nat Nat`.
- Type-level application is identical to term-level application in STLC, except it makes uses of the kinding arrow `=>` from types to types.
- Arrows have the constraint that both the domain and co-domain must be proper types. This prevents non-nonsensical types such as `Pair Nat _ -> Nat`.
- Omega provides a mean to define _types_ from _types_. By using abstraction and application to form types from types. Type operators themselves do not define _terms_ (there is no term of type `\X::*.X`) but are used to form proper types which do have terms: `(\X::*.X) Nat ~>* Nat` which has term `z`.
- System F provides a means to form _terms_ from a type. By providing a type at the _term_ level we can form terms parametrised by that type.
- FOmega combines the power of SystemF and Omega in order to get [higher-order polymorphism](https://en.wikipedia.org/wiki/System_F#System_F%CF%89).
- This implementation follows a [small-step](https://cs.stackexchange.com/questions/43294/difference-between-small-and-big-step-operational-semantics) operational semantics and Berendregt's [variable convention](https://cs.stackexchange.com/questions/69323/barendregts-variable-convention-what-does-it-mean) (see `substitution` in FOmega.hs). The variable convention is adopted for both types and terms.
- Reductions include the one-step reduction (see `reduce1` in FOmega.hs), the many-step reduction (see `reduce` in FOmega.hs). Additionally there is a one-step type-level reduction (see `reduce1T` in FOmega.hs).

## Other Implementation Details
- FOmega.hs contains the Haskell implementation of the calculus, including substitution, reduction, and other useful things.
- Parser.hs contains the monadic parser combinators needed to parse input strings into typed-term ASTs for the calculus.
- Repl.hs contains a simple read-eval-print loop which hooks into main, and into the parser.
- Main.hs is needed for GHC to compile without any flags, it also invokes the repl.

For contributions, see the project to-do list or submit a PR with something you think it needs.
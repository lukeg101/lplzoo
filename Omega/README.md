# Lambda Omega 
Haskell implementation of the Simply Typed Lambda Calculus with type operators. It has STLC terms, a type-level abstraction (with application), kinding, and Nats as an example of well-kinded types.

This language formalises the notion of a type constructor by providing functions from types to types.

## Prerequisites
You need [Haskell](https://www.haskell.org/), this compiles with GHC 8.2.2 at least (Stack resolver: lts-11.0).

## To Build & Run

To compile and run do:
`ghc -O2 -o omega Main`
then run `./omega`

Alternatively to use the GHCi Interpreter do:
`ghci Main`
then type `main`

In either case you get something like the following:
```
Welcome to the Omega REPL
Type some terms or press Enter to leave.
>
```
Note: When run in GHCi, you don't have the luxury of escaped characters, backspace, delete etc...
Compile it using GHC if you need this.

## Examples 
Where you can then have some fun, try these examples:
- `z` This is zero, in Peano style.
- `s (s (s z))` This is 3 in Peano style.
- `\x:Nat. x` This is the identity function on Nats. 
- `\x:(\A::*.A) Nat.x` This is the identity function except with type-level abstraction and application. The type-level abstraction accepts proper types (kinded `*`) such as Nat, and returns a type (of kind `*`) such that the resultant kind is `* => *`. The type-level application applies Nat to the abstraction and returns Nat.

The parser is also smart enough to recognise λ, so you can copy and paste from the output:
```
>   \x:Nat. x
=   λx:Nat.x
>   λx:Nat.x
=   λx:Nat.x
```

`>` denotes the REPL waiting for input, `=` means no reductions occurred (it's the same term), `~>` denotes one reduction, and `~>*` denotes 0 or more reductions (although in practice this is 1 or more due to `=`).

There is also a reduction tracer, which should print each reduction step. prefix any string with `'` in order to see the reductions:
```
```
Reduction occurs at both the term level and type level (inside term-level abstractions as this is where the types are). Here's a function that reduces only at the type level:
```
>   '\x:(\P::(*=>*=>*)=>*.P (\A::*.\B::*.A)) ((\A::*.\B::*.\P::*=>*=>*.P A B) Nat Nat).x
~>  λx:(λA::*.λB::*.λP::*=>*=>*.P A B) Nat Nat (λA::*.λB::*.A).x
~>  λx:(λB::*.λP::*=>*=>*.P Nat B) Nat (λA::*.λB::*.A).x
~>  λx:(λP::*=>*=>*.P Nat Nat) (λA::*.λB::*.A).x
~>  λx:(λA::*.λB::*.A) Nat Nat.x
~>  λx:(λB::*.Nat) Nat.x
~>  λx:Nat.x
```

There is also a typing mechanism, which should display the type or fail as usual.
```
>   t\x:(\A::*.A) Nat.x
(λA::*.A) Nat->(λA::*.A) Nat
>   \x:(\A::*.A).x
Cannot Type Term: λx:λA::*.A.x
```
Note: The above is untypeable as `λA::*.A` is a type operator, such that it takes a type and returns a type (not a term).

Note: if you provide a non-normalizing term, the type checker will fail and reduction will not occur.

You can save variables for the life of the program with a `let` expression. Any time a saved variable appears in a term, it will be substituted for the saved term:
```
Saved term: s (s (s z))
>   let addone = \m:Nat. s m
Saved term: λm:Nat.s m
>   addone three
~>* s (s (s (s z)))
```
Note: Consequently `let` and `=` are keywords, and so you cannot name variables with these.

Note: We include introduction rules for Nats to show how kinding works for proper types. We do not include Nat eliminators however as the focus is on type operators. Use the calculus with this in mind, like the next example.

Additionally we have type level `lett` statements that allow you to define and use types:
```
>   lett PAIR = \A::*.\B::*.\P::*=>*=>*.P A B
Saved type: λA::*.λB::*.λP::*=>*=>*.P A B
>   lett FST = \P::(*=>*=>*)=>*.P (\A::*.\B::*.A)
Saved type: λP::(*=>*=>*)=>*.P (λA::*.λB::*.A)
>   \x:FST (PAIR Nat Nat).x
~>* λx:Nat.x
```
This makes it easier to define both terms and types. `lett` is also a keyword.

## Syntax 

We base the language on the BNF for the typed calculus:

<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}&space;\mathbf{\tau}&&space;::=&space;&&space;\lambda&space;\mathbf{\upsilon}\tt{:}\sigma&space;.&space;\mathbf{\tau}\\&space;&&space;|&space;&&space;\tau\,&space;{\tt&space;space}\,&space;\tau\\&space;&&space;|&space;&&space;\upsilon\\&space;&|&&space;z\\&space;&|&&space;s\,&space;\tau&space;\\&space;&&\\&space;\upsilon&space;&&space;::=&space;&&space;\tt{a}&space;|&space;\tt{b}&space;|&space;\tt{c}&space;|&space;...&space;|&space;\tt{aa}&space;|&space;...&space;\\&space;&&\\&space;\sigma&space;&&space;::=&space;&&space;\lambda&space;\mathbf{\theta&space;}\tt{::}\kappa.\sigma\\&|&&space;\sigma\,{\tt&space;space}\,\sigma\\&space;&|&&space;\theta\\&space;&&space;|&space;&&space;\sigma&space;\rightarrow&space;\sigma&space;\\&space;&|&&space;Nat\\&space;\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}&space;\mathbf{\tau}&&space;::=&space;&&space;\lambda&space;\mathbf{\upsilon}\tt{:}\sigma&space;.&space;\mathbf{\tau}\\&space;&&space;|&space;&&space;\tau\,&space;{\tt&space;space}\,&space;\tau\\&space;&&space;|&space;&&space;\upsilon\\&space;&|&&space;z\\&space;&|&&space;s\,&space;\tau&space;\\&space;&&\\&space;\upsilon&space;&&space;::=&space;&&space;\tt{a}&space;|&space;\tt{b}&space;|&space;\tt{c}&space;|&space;...&space;|&space;\tt{aa}&space;|&space;...&space;\\&space;&&\\&space;\sigma&space;&&space;::=&space;&&space;\lambda&space;\mathbf{\theta&space;}\tt{::}\kappa.\sigma\\&|&&space;\sigma\,{\tt&space;space}\,\sigma\\&space;&|&&space;\theta\\&space;&&space;|&space;&&space;\sigma&space;\rightarrow&space;\sigma&space;\\&space;&|&&space;Nat\\&space;\end{matrix}" title="\begin{matrix} \mathbf{\tau}& ::= & \lambda \mathbf{\upsilon}\tt{:}\sigma . \mathbf{\tau}\\ & | & \tau\, {\tt space}\, \tau\\ & | & \upsilon\\ &|& z\\ &|& s\, \tau \\ &&\\ \upsilon & ::= & \tt{a} | \tt{b} | \tt{c} | ... | \tt{aa} | ... \\ &&\\ \sigma & ::= & \lambda \mathbf{\theta }\tt{::}\kappa.\sigma\\&|& \sigma\,{\tt space}\,\sigma\\ &|& \theta\\ & | & \sigma \rightarrow \sigma \\ &|& Nat\\ \end{matrix}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}&space;\theta&space;&&space;::=&space;&&space;\tt{A}&space;|&space;\tt{B}&space;|&space;\tt{C}&space;|&space;...&space;|&space;\tt{AA}&space;|&space;...\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}&space;\theta&space;&&space;::=&space;&&space;\tt{A}&space;|&space;\tt{B}&space;|&space;\tt{C}&space;|&space;...&space;|&space;\tt{AA}&space;|&space;...\end{matrix}" title="\begin{matrix} \theta & ::= & \tt{A} | \tt{B} | \tt{C} | ... | \tt{AA} | ...\end{matrix}" /></a>

However we adopt standard bracketing conventions to eliminate ambiguity in the parser. Concretely, the parser implements the non-ambiguous grammar as follows:
[TODO]
<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}&space;&&\\&space;\mathbf{\tau}&&space;::=&space;&&space;\lambda&space;\mathbf{\upsilon}\tt{:}\sigma&space;.&space;\mathbf{\tau}\\&space;&&space;|&space;&&space;\alpha\\&space;&&\\&space;\alpha&space;&&space;::=&space;&&space;\beta&space;\\&space;&|&space;&\mathbf{\alpha\,&space;\tt{space}\,&space;\beta}&space;\\&space;&&\\&space;\beta&space;&&space;::=&space;&&space;\tt{(}\tau&space;\tt{)}\\&space;&|&&space;\upsilon&space;\\&space;&&\\&space;\upsilon&space;&&space;::=&space;&&space;\tt{a}&space;|&space;\tt{b}&space;|&space;\tt{c}&space;|&space;...&space;|&space;\tt{aa}&space;|&space;...&space;&&\\&space;\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}&space;&&\\&space;\mathbf{\tau}&&space;::=&space;&&space;\lambda&space;\mathbf{\upsilon}\tt{:}\sigma&space;.&space;\mathbf{\tau}\\&space;&&space;|&space;&&space;\alpha\\&space;&&\\&space;\alpha&space;&&space;::=&space;&&space;\beta&space;\\&space;&|&space;&\mathbf{\alpha\,&space;\tt{space}\,&space;\beta}&space;\\&space;&&\\&space;\beta&space;&&space;::=&space;&&space;\tt{(}\tau&space;\tt{)}\\&space;&|&&space;\upsilon&space;\\&space;&&\\&space;\upsilon&space;&&space;::=&space;&&space;\tt{a}&space;|&space;\tt{b}&space;|&space;\tt{c}&space;|&space;...&space;|&space;\tt{aa}&space;|&space;...&space;&&\\&space;\end{matrix}" title="\begin{matrix} &&\\ \mathbf{\tau}& ::= & \lambda \mathbf{\upsilon}\tt{:}\sigma . \mathbf{\tau}\\ & | & \alpha\\ &&\\ \alpha & ::= & \beta \\ &| &\mathbf{\alpha\, \tt{space}\, \beta} \\ &&\\ \beta & ::= & \tt{(}\tau \tt{)}\\ &|& \upsilon \\ &&\\ \upsilon & ::= & \tt{a} | \tt{b} | \tt{c} | ... | \tt{aa} | ... &&\\ \end{matrix}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}&space;&&\\&space;\mathbf{\sigma}&&space;::=&space;&&space;\lambda&space;\mathbf{\theta}\tt{::}\kappa&space;.&space;\mathbf{\sigma}\\&space;&&space;|&space;&&space;\eta\\&space;&&\\&space;\eta&space;&&space;::=&space;&&space;\eta&space;\rightarrow&space;\delta\\&space;&|&\delta&space;\\&space;&&\\&space;\delta&space;&&space;::=&space;&\mathbf{\delta\,&space;\tt{space}\,\epsilon&space;}&space;\\&|&&space;\epsilon\\\\&space;\epsilon&space;&&space;::=&space;&\tt{(}\sigma&space;\tt{)}\\&space;&|&&space;\theta&space;\\\\&space;\theta&space;&&space;::=&space;&&space;\tt{A}&space;|&space;\tt{B}&space;|&space;\tt{C}&space;|&space;...&space;|&space;\tt{AA}&space;|&space;...&space;&&\\&space;\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}&space;&&\\&space;\mathbf{\sigma}&&space;::=&space;&&space;\lambda&space;\mathbf{\theta}\tt{::}\kappa&space;.&space;\mathbf{\sigma}\\&space;&&space;|&space;&&space;\eta\\&space;&&\\&space;\eta&space;&&space;::=&space;&&space;\eta&space;\rightarrow&space;\delta\\&space;&|&\delta&space;\\&space;&&\\&space;\delta&space;&&space;::=&space;&\mathbf{\delta\,&space;\tt{space}\,\epsilon&space;}&space;\\&|&&space;\epsilon\\\\&space;\epsilon&space;&&space;::=&space;&\tt{(}\sigma&space;\tt{)}\\&space;&|&&space;\theta&space;\\\\&space;\theta&space;&&space;::=&space;&&space;\tt{A}&space;|&space;\tt{B}&space;|&space;\tt{C}&space;|&space;...&space;|&space;\tt{AA}&space;|&space;...&space;&&\\&space;\end{matrix}" title="\begin{matrix} &&\\ \mathbf{\sigma}& ::= & \lambda \mathbf{\theta}\tt{::}\kappa . \mathbf{\sigma}\\ & | & \eta\\ &&\\ \eta & ::= & \eta \rightarrow \delta\\ &|&\delta \\ &&\\ \delta & ::= &\mathbf{\delta\, \tt{space}\,\epsilon } \\&|& \epsilon\\\\ \epsilon & ::= &\tt{(}\sigma \tt{)}\\ &|& \theta \\\\ \theta & ::= & \tt{A} | \tt{B} | \tt{C} | ... | \tt{AA} | ... &&\\ \end{matrix}" /></a>


Some notes about the syntax:

[TODO]
- The above syntax only covers the core calculus, and not the repl extensions (such as let bindings above). The extensions are simply added on in the repl.
- Term and type variables are strings (excluding numbers), as this is isomorphic to a whiteboard treatment and hence the most familiar. Term variables are lower case whereas type variables are upper case.
- All term level syntax is identical to STLC, however the valid types are different.
- Types are variables `A,B,C,...`, type operators/abstractions `\X::K.T`, applications `A B`, or arrows `A -> B`. There is also the `Nat` type.
- Omega formally introduces type variables `A, B, C...` which until now are used informally for convenience. STLC does not have variables for this reason.
- Arrows associate to the right so that `X -> Y -> Z` is the same as `X -> (Y -> Z)` but not `((X -> Y) -> Z)`.
- For both term and type abstractions nested terms don't need brackets: `\x:Nat.\y:Nat. y` unless enforcing application on the right. Similarly at both levels whitespace does not matter `(\x:Nat.          x)` unless it is between application where you need at least one space.
- To quit use `Ctrl+C` or whatever your machine uses to interrupt computations.

## Semantics

The semantics implements beta-reduction on terms and alpha-equivalence as the `Eq` instance of `OTerm`. The semantics are the same as the untyped calculus with the addition of types and kinds. We reformulate the semantics as [typing judgements](https://existentialtype.wordpress.com/2011/03/27/the-holy-trinity/):

for term-level variables:

<a href="https://www.codecogs.com/eqnedit.php?latex=\overline{\Gamma&space;\vdash&space;x:T},\quad&space;\mbox{(if&space;$x:T&space;\in&space;\Gamma$)}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\overline{\Gamma&space;\vdash&space;x:T},\quad&space;\mbox{(if&space;$x:T&space;\in&space;\Gamma$)}" title="\overline{\Gamma \vdash x:T},\quad \mbox{(if $x:T \in \Gamma$)}" /></a>

for term-level abstractions, with the added constraint that the variable in the abstraction is a proper type (`*`):

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{\Gamma&space;\vdash&space;A&space;::&space;*&space;\quad&space;\Gamma&space;\vdash&space;f&space;:&space;A&space;\Rightarrow&space;B\quad&space;\Gamma&space;\vdash&space;x&space;:&space;A}{\Gamma&space;\vdash&space;(f&space;x)&space;:&space;B}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{\Gamma&space;\vdash&space;A&space;::&space;*&space;\quad&space;\Gamma&space;\vdash&space;f&space;:&space;A&space;\Rightarrow&space;B\quad&space;\Gamma&space;\vdash&space;x&space;:&space;A}{\Gamma&space;\vdash&space;(f&space;x)&space;:&space;B}" title="\frac{\Gamma \vdash A :: * \quad \Gamma \vdash f : A \Rightarrow B\quad \Gamma \vdash x : A}{\Gamma \vdash (f x) : B}" /></a>

and term-level application:

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{\Gamma&space;\vdash&space;f&space;:&space;A&space;\Rightarrow&space;B\quad&space;\Gamma&space;\vdash&space;x&space;:&space;A}{\Gamma&space;\vdash&space;(f&space;x)&space;:&space;B}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{\Gamma&space;\vdash&space;f&space;:&space;A&space;\Rightarrow&space;B\quad&space;\Gamma&space;\vdash&space;x&space;:&space;A}{\Gamma&space;\vdash&space;(f&space;x)&space;:&space;B}" title="\frac{\Gamma \vdash f : A \Rightarrow B\quad \Gamma \vdash x : A}{\Gamma \vdash (f x) : B}" /></a>

for zero and succ:

<a href="https://www.codecogs.com/eqnedit.php?latex=\overline{\Gamma&space;\vdash&space;z:Nat}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\overline{\Gamma&space;\vdash&space;z:Nat}" title="\overline{\Gamma \vdash z:Nat}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{\Gamma&space;\vdash&space;n&space;:&space;Nat}{\Gamma&space;\vdash&space;s\,&space;n&space;:&space;Nat}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{\Gamma&space;\vdash&space;n&space;:&space;Nat}{\Gamma&space;\vdash&space;s\,&space;n&space;:&space;Nat}" title="\frac{\Gamma \vdash n : Nat}{\Gamma \vdash s\, n : Nat}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{n&space;\rightsquigarrow&space;n'}{s\,&space;n&space;\rightsquigarrow&space;s\,n'}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{n&space;\rightsquigarrow&space;n'}{s\,&space;n&space;\rightsquigarrow&space;s\,n'}" title="\frac{n \rightsquigarrow n'}{s\, n \rightsquigarrow s\,n'}" /></a>

and the reduction relation adopted from the untyped theory (with types added in the abstraction):

<a href="https://www.codecogs.com/eqnedit.php?latex=(\lambda&space;x&space;:&space;T&space;.&space;M)N&space;\rightsquigarrow&space;M&space;[x&space;:=&space;N]" target="_blank"><img src="https://latex.codecogs.com/gif.latex?(\lambda&space;x&space;:&space;T&space;.&space;M)N&space;\rightsquigarrow&space;M&space;[x&space;:=&space;N]" title="(\lambda x : T . M)N \rightsquigarrow M [x := N]" /></a>

We now have abstraction, application and type-variables at the type level with 'types of types' known as kinds. This means the semantics of types mirror term-level semantics of STLC but one level up:

For type-variables:

<a href="https://www.codecogs.com/eqnedit.php?latex=\overline{\Gamma&space;\vdash&space;X&space;::&space;K},&space;\mbox{(if&space;$X&space;::&space;K&space;\in&space;\Gamma$)}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\overline{\Gamma&space;\vdash&space;X&space;::&space;K},&space;\mbox{(if&space;$X&space;::&space;K&space;\in&space;\Gamma$)}" title="\overline{\Gamma \vdash X :: K}, \mbox{(if $X :: K \in \Gamma$)}" /></a>

For type-abstraction (known as a type operator):

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{\Gamma,X&space;::&space;K_1&space;\vdash&space;Y&space;::&space;K_2\quad}{\Gamma&space;\vdash&space;\lambda&space;X&space;::&space;K_1&space;.&space;Y&space;::&space;K_2}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{\Gamma,X&space;::&space;K_1&space;\vdash&space;Y&space;::&space;K_2\quad}{\Gamma&space;\vdash&space;\lambda&space;X&space;::&space;K_1&space;.&space;Y&space;::&space;K_2}" title="\frac{\Gamma,X :: K_1 \vdash Y :: K_2\quad}{\Gamma \vdash \lambda X :: K_1 . Y :: K_2}" /></a>

For type-application:

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{\Gamma&space;\vdash&space;F&space;::&space;K_1&space;\Rightarrow&space;K_2\quad\Gamma\vdash&space;X&space;:&space;:K_1}{\Gamma&space;\vdash&space;F\,X&space;::&space;K_2}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{\Gamma&space;\vdash&space;F&space;::&space;K_1&space;\Rightarrow&space;K_2\quad\Gamma\vdash&space;X&space;:&space;:K_1}{\Gamma&space;\vdash&space;F\,X&space;::&space;K_2}" title="\frac{\Gamma \vdash F :: K_1 \Rightarrow K_2\quad\Gamma\vdash X : :K_1}{\Gamma \vdash F\,X :: K_2}" /></a>

For the existing type arrow (now with constraints that both sides of the arrow must have proper types):

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{\Gamma&space;\vdash&space;T_1&space;::&space;*\quad\Gamma\vdash&space;T_2&space;:&space;:*}{\Gamma&space;\vdash&space;T_1&space;\rightarrow&space;T_2::*}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{\Gamma&space;\vdash&space;T_1&space;::&space;*\quad\Gamma\vdash&space;T_2&space;:&space;:*}{\Gamma&space;\vdash&space;T_1&space;\rightarrow&space;T_2::*}" title="\frac{\Gamma \vdash T_1 :: *\quad\Gamma\vdash T_2 : :*}{\Gamma \vdash T_1 \rightarrow T_2::*}" /></a>

Next, Nats are a proper type: 

-<a href="https://www.codecogs.com/eqnedit.php?latex=\overline{\Gamma&space;\vdash&space;Nat::*}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\overline{\Gamma&space;\vdash&space;Nat::*}" title="\overline{\Gamma \vdash Nat::*}" /></a>

Lastly, we have type-level beta reduction, identical to STLC reduction but for types:

<a href="https://www.codecogs.com/eqnedit.php?latex=(\lambda&space;X&space;::&space;K&space;.&space;T_1)\,T_2&space;\rightsquigarrow&space;T_1&space;[X&space;:=&space;T_2]" target="_blank"><img src="https://latex.codecogs.com/gif.latex?(\lambda&space;X&space;::&space;K&space;.&space;T_1)\,T_2&space;\rightsquigarrow&space;T_1&space;[X&space;:=&space;T_2]" title="(\lambda X :: K . T_1)\,T_2 \rightsquigarrow T_1 [X := T_2]" /></a>

- This means the typing context now also contains types, and types occur in terms. The phrase `X :: K` means X is has kind K. We do not implement Agda style type hierarchies here.
-There is the additional constraint that any term abstractions must take a term of proper type. This is in order to prevent nonsensical or partially applied types terms such as `Nat Nat`.
-This reflects the principle that any typeable term has a kindable type.
-The calculus on its own does not have any proper types (such as Nat, Bool or Unit) and so the base calculus is unusable as no terms can be introduced. To demonstrate proper types, we add the proper type Nat, with constructors `z` and `s`.
-Type abstractions can be of any kind permitted by Omega, however the resultant type must be proper for it to be accepted by a term-level abstraction.
-Type-level application is identical to term-level application STLC, except is makes uses of a kinding arrow `=>` from types to types.
-Arrows have the constraint that both the domain and co-domain must be proper types. This prevents non-nonsensical terms such as `Pair Nat _ -> Nat`.
-The term-level calculus is identical to STLC.
- This implementation follows a [small-step](https://cs.stackexchange.com/questions/43294/difference-between-small-and-big-step-operational-semantics) operational semantics and Berendregt's [variable convention](https://cs.stackexchange.com/questions/69323/barendregts-variable-convention-what-does-it-mean) (see `substitution` in Omega.hs). 
- Reductions include the one-step reduction (see `reduce1` in Omega.hs), the many-step reduction (see `reduce` in Omega.hs). additionally there is a one-step type-level reduction (see `reduce1T` in Omega.hs).

## Other Implementation Details
- Omega.hs contains the Haskell implementation of the calculus, including substitution, reduction, and other useful things.
- Parser.hs contains the monadic parser combinators needed to parse input strings into typed-term ASTs for the calculus.
- Repl.hs contains a simple read-eval-print loop which hooks into main, and into the parser.
- Main.hs is needed for GHC to compile without any flags, it also invokes the repl.

For contributions, see the [project to-do list](https://github.com/lukeg101/lplzoo/projects/2) or submit a PR with something you think it needs.

Work initially documented [here](https://gist.github.com/lukeg101/ac1f2319e965f600b2e8b45a372f5fda).



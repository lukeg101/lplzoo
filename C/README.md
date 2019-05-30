# The Calculus of Constructions
Haskell implementation of Thierry Coquand and Gerard Huet's [Calculus of Constructions](https://core.ac.uk/download/pdf/82038778.pdf). In essence it is the apex of Berendregt's [lambda cube](https://en.wikipedia.org/wiki/Lambda_cube), capturing the expressiveness of STLC, System F, Omega, and LF. This calculus is presented as a [pure type system](https://www.researchgate.net/publication/216300104_An_Introduction_to_Generalized_Type_Systems) and is hence incredibly simple in representation with universal quantification doubling up as the Pi type, abstraction, 2 sorts (* and box), and application.  

This strongly normalising calculus This allows us to model [higher-order predicate logic](https://en.wikipedia.org/wiki/Higher-order_logic) in a language, and hence do proof in programs. More importantly it can serve as a constructive foundation of mathematics and hence make the link between proof and program explicit.

## Prerequisites
You need [Haskell](https://www.haskell.org/), this compiles with GHC 8.2.2 at least (Stack resolver: lts-11.0).

## To Build & Run

To compile and run do:
`ghc -O2 -o c Main`
then run `./c`

Alternatively to use the GHCi Interpreter do:
`ghci Main`
then type `main`

In either case you get something like the following:
```
Welcome to the C REPL
Type some terms or press Enter to leave.
>
```

Note: When run in GHCi, you don't have the luxury of escaped characters, backspace, delete etc...
Compile it using GHC if you need this.

## Examples 
Where you can then have some fun, try these examples:
- `Pi x:*.\y:x.x` identity function on proper types.
- `Pi n:*.(n->n)->n->n` church encoding of Nats.
- `(\x:*.x) (Pi x:*.(x -> x) -> x -> x)` identity function for nats
- `\X:*.\x:X.x` polymorphic identity function
- `\x:*.\f:x->x.\z:x.z` Church-encoding of Peano style zero 
- `\n:(Pi n:*.(n->n)->n->n).\A:*.\f:A->A.\z:A.f (n A f z)` succ for church encoded nats
- `λn:Π A:*.(A->A)->A->A.λm:Π A:*.(A->A)->A->A.n (Π A:*.(A->A)->A->A) (λn:Π n:*.(n->n)->n->n.λA:*.λf:A->A.λz:A.f (n A f z)) m` plus for church encoded nats
- `Pi A:*.Pi B:*.A -> B` encoding of A implies B from propositional logic
- `Pi A:*.Pi B:*.Pi C:*.(A -> B -> C) -> C` encoding of tuples A and B
- `Pi A:*.Pi B:*.Pi C:*.(A -> B) -> (B -> C) -> C` encoding of disjoint unions
- `\A:*.\x:A.\y:A.Pi p:A->*.(p x) -> p y` equality for terms of a type.
- `\A:*.\x:A.Pi p:A->*.(p x) -> p x` the reflexivity of equality.
- for examples of dependent typing, see the assume examples below.

Note: `Π` is the dependent product type and `Πx:A.B` is the same as `A -> B` when `x` does not occur in `B`. `Π` is alternatively typed as `Pi`, respectively. `*` is the sort of proper types, it has type `□` but `□` isn't part of the syntax as it is the maximal sort.

The parser is smart enough to recognise λ, Π , and *; so you can copy and paste from the output:
```
>   Pi x:*.\y:x.x
=   Π x:*.λy:x.x
>   Π x:*.λy:x.x
=   Π x:*.λy:x.x
```

`>` denotes the REPL waiting for input, `=` means no reductions occurred (it's the same term), `~>` denotes one reduction, and `~>*` denotes 0 or more reductions (although in practice this is 1 or more due to `=`).

There is also a reduction tracer, which should print each reduction step. prefix any string with `'` in order to see the reductions:
```
>   '(\n:(Pi n:*.(n->n)->n->n).\A:*.\f:A->A.\z:A.f (n A f z)) (\x:*.\f:x->x.\z:x.z)
~>  λA:*.λf:A->A.λz:A.f ((λx:*.λf:x->x.λz:x.z) A f z)
~>  λA:*.λf:A->A.λz:A.f ((λf:A->A.λz:A.z) f z)
~>  λA:*.λf:A->A.λz:A.f ((λz:A.z) z)
~>  λA:*.λf:A->A.λz:A.f z
```
Note: this is `succ 0` in peano arithmetic.

There is also a typing mechanism, which should display the type or fail as usual.
```
>   t\X:*.\x:X.x x
Cannot Type Term: λX:*.λx:X.x x
>   t\X:*.\x:X.x
Π X:*.X->X
```

Note: if you provide a non-normalizing term, the type checker will fail and reduction will not occur.

You can save terms for the life of the program with a `let` expression. Any time a saved variable appears in a term, it will be substituted for the saved term:
```
>   let nat = Pi N:*.(N->N)->N->N
Saved term: nat = Π N:*.(N->N)->N->N
>   let 0 = \X:*.\f:X->X.\z:X.z
Saved term: 0 = λX:*.λf:X->X.λz:X.z
>   let s = \n:nat.\A:*.\f:A->A.\z:A.f (n A f z)
Saved term: s = λn:Π N:*.(N->N)->N->N.λA:*.λf:A->A.λz:A.f (n A f z)
>   s 0
~>* λA:*.λf:A->A.λz:A.f z
>   let plus = λn:nat.λm:nat.n nat (λn:nat.λA:*.λf:A->A.λz:A.f (n A f z)) m
Saved term: plus = λn:Π N:*.(N->N)->N->N.λm:Π N:*.(N->N)->N->N.n (Π N:*.(N->N)->N->N) (λn:Π N:*.(N->N)->N->N.λA:*.λf:A->A.λz:A.f (n A f z)) m
>   plus (s 0) (s 0)
~>* λA:*.λf:A->A.λz:A.f (f z)
```

try to prove that `plus n 0 = n`. You'll need to invent an nat induction term, and make use of equality and refl defined above.

Note: Consequently `let` and `=` are keywords, and so you cannot name variables with these. Additionally `Pi` and `assume` are keywords.

We have the ability to embed types, constructors, or type families into the typing context with assume statements:
```
>   assume Bool : *
Saved typing: Bool:*
>   assume true : Bool
Saved typing: true:Bool
>   assume Vec : Pi X:*. Pi n:nat.*
Saved typing: Vec:*->(Π N:*.(N->N)->N->N)->*
>   Vec Bool 0
=   Vec Bool (λX:*.λf:X->X.λz:X.z)
>   tVec Bool 0
*
>   assume nil : Vec Bool 0
Saved typing: nil:Vec Bool (λX:*.λf:X->X.λz:X.z)
>   assume cons : Pi n:nat.Bool -> (Vec Bool n) -> (Vec Bool (s n))
Saved typing: cons:Π n:Π N:*.(N->N)->N->N.Bool->(Vec Bool n)->Vec Bool ((λn:Π N:*.(N->N)->N->N.λA:*.λf:A->A.λz:A.f (n A f z)) n)
>   assume cons : Pi n:nat.Bool -> (Vec Bool n) -> (Vec Bool (s n))
Saved typing: cons:Π n:Π N:*.(N->N)->N->N.Bool->(Vec Bool n)->Vec Bool ((λn:Π N:*.(N->N)->N->N.λA:*.λf:A->A.λz:A.f (n A f z)) n)
>   cons 0 true nil
=   cons (λX:*.λf:X->X.λz:X.z) true nil
>   tcons 0 true nil
Vec Bool (λA:*.λf:A->A.λz:A.f z)
```
Note: `assume` is not part of the core calculus and if you use it in the wrong way you may run into theoretical problems. It is possible to design a fully polymorphic vector type using just `let` and plumbing, but assumes give us syntactic freedom that makes this language fun to use. Parsing and typing is also done here so you must ensure your terms are well-typed.

this allows us to encode new types and their constructors. Consequently `assume` is also a keyword.

## Syntax 

We base the language on the BNF for the typed calculus:

<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}&space;\mathbf{\tau}&&space;::=&space;&&space;\lambda&space;\mathbf{\upsilon}{\tt&space;:}\tau&space;.&space;\mathbf{\tau}\\&space;&|&&space;\Pi&space;\mathbf{\upsilon}{\tt&space;:}\tau&space;.&space;\mathbf{\tau}\\&space;&|&&space;\tau&space;\rightarrow&space;\tau\\&space;&&space;|&space;&&space;\tau\,&space;\tau\\&space;&&space;|&space;&&space;\upsilon&space;\\&space;&|&&space;*\\&space;&&\\&space;\upsilon&space;&&space;::=&space;&&space;\tt{a}&space;|&space;\tt{b}&space;|&space;...&space;|&space;\tt{A}&space;|&space;{\tt&space;B}&space;|...&space;|0|1|...\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}&space;\mathbf{\tau}&&space;::=&space;&&space;\lambda&space;\mathbf{\upsilon}{\tt&space;:}\tau&space;.&space;\mathbf{\tau}\\&space;&|&&space;\Pi&space;\mathbf{\upsilon}{\tt&space;:}\tau&space;.&space;\mathbf{\tau}\\&space;&|&&space;\tau&space;\rightarrow&space;\tau\\&space;&&space;|&space;&&space;\tau\,&space;\tau\\&space;&&space;|&space;&&space;\upsilon&space;\\&space;&|&&space;*\\&space;&&\\&space;\upsilon&space;&&space;::=&space;&&space;\tt{a}&space;|&space;\tt{b}&space;|&space;...&space;|&space;\tt{A}&space;|&space;{\tt&space;B}&space;|...&space;|0|1|...\end{matrix}" title="\begin{matrix} \mathbf{\tau}& ::= & \lambda \mathbf{\upsilon}{\tt :}\tau . \mathbf{\tau}\\ &|& \Pi \mathbf{\upsilon}{\tt :}\tau . \mathbf{\tau}\\ &|& \tau \rightarrow \tau\\ & | & \tau\, \tau\\ & | & \upsilon \\ &|& *\\ &&\\ \upsilon & ::= & \tt{a} | \tt{b} | ... | \tt{A} | {\tt B} |... |0|1|...\end{matrix}" /></a>

However we adopt standard bracketing conventions to eliminate ambiguity in the parser. Concretely, the parser implements the non-ambiguous grammar for terms as follows:

<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}&space;\mathbf{\tau}&&space;::=&space;&&space;\lambda&space;\mathbf{\upsilon}\tt{:}\sigma&space;.&space;\mathbf{\tau}\\&space;&|&\Pi\upsilon:&space;\tau\\&space;&|&&space;\alpha&space;\rightarrow&space;\tau\\&space;&&space;|&space;&&space;\alpha\\&space;&&\\&space;\alpha&space;&&space;::=&space;&&space;\beta&space;\\&space;&|&space;&\mathbf{\alpha\,&space;\tt{space}\,&space;\beta}&space;\\&space;&&\\&space;\beta&space;&&space;::=&space;&&space;\tt{(}\tau&space;\tt{)}\\&space;&|&&space;\upsilon\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}&space;\mathbf{\tau}&&space;::=&space;&&space;\lambda&space;\mathbf{\upsilon}\tt{:}\sigma&space;.&space;\mathbf{\tau}\\&space;&|&\Pi\upsilon:&space;\tau\\&space;&|&&space;\alpha&space;\rightarrow&space;\tau\\&space;&&space;|&space;&&space;\alpha\\&space;&&\\&space;\alpha&space;&&space;::=&space;&&space;\beta&space;\\&space;&|&space;&\mathbf{\alpha\,&space;\tt{space}\,&space;\beta}&space;\\&space;&&\\&space;\beta&space;&&space;::=&space;&&space;\tt{(}\tau&space;\tt{)}\\&space;&|&&space;\upsilon\end{matrix}" title="\begin{matrix} \mathbf{\tau}& ::= & \lambda \mathbf{\upsilon}\tt{:}\sigma . \mathbf{\tau}\\ &|&\Pi\upsilon: \tau\\ &|& \alpha \rightarrow \tau\\ & | & \alpha\\ &&\\ \alpha & ::= & \beta \\ &| &\mathbf{\alpha\, \tt{space}\, \beta} \\ &&\\ \beta & ::= & \tt{(}\tau \tt{)}\\ &|& \upsilon\end{matrix}" /></a>

Some notes about the syntax:

- The above syntax only covers the core calculus, and not the repl extensions (such as let/assume bindings above). The extensions are simply added on in the repl.
- Term variables are strings, as this is isomorphic to a whiteboard treatment and hence the most familiar. Unlike previous languages in the zoo the constraint on variables is relaxed so that you may make alpha-numeric terms.
- In line with pure type systems, the type/term abstraction is collapsed so that we have a single syntactic category of terms. Terms are variables (`a,b,c,...`), abstractions (`\x:t1.t2`), Pi abstractions (`Pi n:t1.t2`), applications `t1 t2`, and sorts (`*`).
- Typing is thus a transformation from terms to terms, where variables parametrise terms through abstraction, and instantiate others through application. 
- To prevent cyclic and otherwise theoretical complications there is a sort `*` of well-typed terms, box for well-formed kinds. This allows us to establish a hierarchy of terms.
- Arrows `A -> B` terms `Pi x:A.B` where x doesn't occur in `B`. This is syntactic shorthand and is only present in the pretty printer. 
- Box is not part of the syntax as it is only used in typing.
- To quit use `Ctrl+C` or whatever your machine uses to interrupt computations.

## Semantics

TODO

TODO
- This implementation follows a [small-step](https://cs.stackexchange.com/questions/43294/difference-between-small-and-big-step-operational-semantics) operational semantics and Berendregt's [variable convention](https://cs.stackexchange.com/questions/69323/barendregts-variable-convention-what-does-it-mean) (see `substitution` in C.hs).
- Reductions include the one-step reduction (see `reduce1` in C.hs), the many-step reduction (see `reduce` in C.hs). Additionally there is a one-step type-level reduction (see `reduce1T` in C.hs).

## Other Implementation Details
- C.hs contains the Haskell implementation of the calculus, including substitution, reduction, and other useful things.
- Parser.hs contains the monadic parser combinators needed to parse input strings into typed-term ASTs for the calculus.
- Repl.hs contains a simple read-eval-print loop which hooks into main, and into the parser.
- Main.hs is needed for GHC to compile without any flags, it also invokes the repl.

For contributions, see the project to-do list or submit a PR with something you think it needs.
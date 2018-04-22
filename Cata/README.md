# The Cata(morphic) Programming Language
Haskell implementation of the simply typed lambda calculus with inductive types. It ranges over base types `A,B,C...`, the function type `T->T`, Products, Sums, Units, and has a special `μ` type (with `μ` variable `X`) to define [recursive types](https://en.wikipedia.org/wiki/Recursive_data_type). It is strongly normalizing due to the well-founded nature of inductive types but not Turing Complete.

Inductive types are an important stepping-stone towards [algebraic datatypes](https://en.wikipedia.org/wiki/Algebraic_data_type) in functional languages and a powerful tool to use with [inductive equational reasoning](https://en.wikipedia.org/wiki/Structural_induction). 

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
TODO
```

Note: if you provide a un-typeable term, the type checker will fail and reduction will not occur.

## Syntax 

We base the language on the BNF for Cata:

However we adopt standard bracketing conventions to eliminate ambiguity in the parser. Concretely, the parser implements the non-ambiguous grammar as follows:

TODO 

Some notes about the syntax:

- Variables are positive integers (including zero) as this is easy for Haskell to process, and for me implement variable generation. This is isomorphic to a whiteboard treatment using characters (like `\x:A.x`).
- Types range over upper-case characters `A,B,C...`, nested arrow types: `T -> T`, product types `A * B` (or `×`), sum types `A + B`, unit types `1` (or `⊤`), a special Mu type `μ(T)` (or `M(T)`), and Mu type variable `X`.  
- Arrows associate to the right so that `A -> A -> A` is the same as `A -> (A -> A)` but not `((A -> A) -> A)`. Similar rules follow for the other types.
- Products have the highest precedence, followed by sums, arrows, and then all other types. 
- Nested terms don't need brackets: `\1:A.\2:B. 2` unless enforcing application on the right. Whitespace does not matter `(\1:A.          1)` unless it is between application where you need at least one space.
- Products are 2-element pairs like in Haskell. You form products like `(1, 2)` and access each element using `fst` (or `π1`) and `snd` (or `π2`).
- Sums are 2-element co-pairs, formed with either `inl 1:A` or `inr 2:B` (assuming either `1:A` or `2:B` is in scope). Do case analysis using `case c f g` performs case analysis on the first argument to `case`, passing the result to the function `f` if `inl` or otherwise `g` if `inr`.
- Units are largely uninteresting, but can be formed as `()` like in Haskell, and typed like `1` or (or `⊤`).
- Inductive types are formed by prefixing a term with `in` and a type with `M` (or `μ`). Concretely, that means a term would like something like `in x:t` where `x` is the term and `t` is its type. Similarly, an inductive type looks like `μ (T)` where `T` is the type we insist is inductive. For example, inductively defined nats have the type `μ(1+X)` where `1` is the base case, and `X` is a μ variable capturing the subterm (also a nat). The term zero above captures this idea.

TODO

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

TODO

for sums:

TODO 

and for units:

TODO

Finally, we have rules for `μ` type introduction, elimination and reduction. Note how this forms a [fold](https://en.wikipedia.org/wiki/Fold_(higher-order_function)):

TODO

- This implementation follows a [small-step](https://cs.stackexchange.com/questions/43294/difference-between-small-and-big-step-operational-semantics) operational semantics and Berendregt's [variable convention](https://cs.stackexchange.com/questions/69323/barendregts-variable-convention-what-does-it-mean) (see `substitution` in Cata.hs). 
- Reductions include the one-step reduction (see `reduce1` in Cata.hs), the many-step reduction (see `reduce` in Cata.hs). 

## Other Implementation Details
- Cata.hs contains the Haskell implementation of the calculus, including substitution, reduction, and other useful things.
- Parser.hs contains the monadic parser combinators needed to parse input strings into typed-term ASTs for the calculus.
- Repl.hs contains a simple read-eval-print loop which hooks into main, and into the parser.
- Main.hs is needed for GHC to compile without any flags, it also invokes the repl.

For contributions, see the project to-do list or submit a PR with something you think it needs.


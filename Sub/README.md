# Simply-Typed Lambda Calculus with Subtyping
Haskell implementation on Benjamin Pierce's Lambda Calculus with Subtyping. It has the features of STLC but with generalised records, a Unit type, and subtype polymorphism.

This strongly normalising calculus serves as a foundation for object-oriented  languages such as Java, Scala, and Python.

## Prerequisites
You need [Haskell](https://www.haskell.org/), this compiles with GHC 8.2.2 at least (Stack resolver: lts-11.0).

## To Build & Run

To compile and run do:
`ghc -O2 -o sub Main`
then run `./sub`

Alternatively to use the GHCi Interpreter do:
`ghci Main`
then type `main`

In either case you get something like the following:
```
Welcome to the Sub REPL
Type some terms or press Enter to leave.
>
```

Note: When run in GHCi, you don't have the luxury of escaped characters, backspace, delete etc...
Compile it using GHC if you need this.

## Examples 
Where you can then have some fun, try these examples:
-TODO
-TODO
-TODO 

The parser is smart enough to recognise λ; so you can copy and paste from the output:
```
TODO
```

There is also a reduction tracer, which should print each reduction step. prefix any string with `'` in order to see the reductions:
```
TODO
```
There is also a typing mechanism, which should display the type or fail as usual.
```
TODO
```

Note: if you provide a non-normalizing term, the type checker will fail and reduction will not occur.

## Syntax 

We base the language on the BNF for the typed calculus:

TODO

However we adopt standard bracketing conventions to eliminate ambiguity in the parser. Concretely, the parser implements the non-ambiguous grammar for terms as follows:

TODO 

and types:

TODO

with variables:

TODO

Some notes about the syntax:

- Variables are positive integers (including zero) as this is easy for Haskell to process, and for me implement variable generation. This is isomorphic to a whiteboard treatment using characters (like `\x:1.x`).
- Types are also positive integers for the same reasons. Type variables should be distinct from term variables, although it is not prohibited: The term `TODO` is valid but less readable due to the dual use of `1`.
- Types are either type variables, abstractions, or nested arrow types: `T -> T`. Arrows associate to the right so that `T -> T -> T` is the same as `T -> (T -> T)` but not `((T -> T) -> T)`. 
- Nested terms don't need brackets: `\1:3.\2:3. 2` unless enforcing application on the right. Whitespace does not matter `TODO` unless it is between application where you need at least one space.
- TODO subtyping
- To quit use `Ctrl+C` or whatever your machine uses to interrupt computations.

## Semantics

The semantics implements beta-reduction on terms and alpha-equivalence as the `Eq` instance of `STerm`. The semantics are the same as STLC with the addition of subtype polymorphism with augments the typing relation with a subtyping relation `<`. We reformulate the semantics as [typing judgements](https://existentialtype.wordpress.com/2011/03/27/the-holy-trinity/):

We have typing rules for the Unit type (otherwise known as Top):

TODO

The subtyping relation is a reflexive and transitive relation `<` defined as follows:

These rules denote reflexivity, transitivity, that `()` is the _supertype_ of all types, and the arrow-subtyping rule for functions. The standard typing rules from STLC are then influenced by this:

TODO

for abstractions:

TODO

for application:

TODO

and the reduction relation (adopted from STLC):

<a href="https://www.codecogs.com/eqnedit.php?latex=(\lambda&space;x&space;:&space;T&space;.&space;M)N&space;\rightsquigarrow&space;M&space;[x&space;:=&space;N]" target="_blank"><img src="https://latex.codecogs.com/gif.latex?(\lambda&space;x&space;:&space;T&space;.&space;M)N&space;\rightsquigarrow&space;M&space;[x&space;:=&space;N]" title="(\lambda x : T . M)N \rightsquigarrow M [x := N]" /></a>

and special introduction, elimination, and reduction rules for records:

TODO

- This implementation follows a [small-step](https://cs.stackexchange.com/questions/43294/difference-between-small-and-big-step-operational-semantics) operational semantics and Berendregt's [variable convention](https://cs.stackexchange.com/questions/69323/barendregts-variable-convention-what-does-it-mean) (see `substitution` in Sub.hs). The variable convention is adopted for both types and terms.
- Reductions include the one-step reduction (see `reduce1` in Sub.hs), the many-step reduction (see `reduce` in Sub.hs). 

## Other Implementation Details
- Sub.hs contains the Haskell implementation of the calculus, including substitution, subtyping, reduction, and other useful things.
- Parser.hs contains the monadic parser combinators needed to parse input strings into typed-term ASTs for the calculus.
- Repl.hs contains a simple read-eval-print loop which hooks into main, and into the parser.
- Main.hs is needed for GHC to compile without any flags, it also invokes the repl.

For contributions, see the project to-do list or submit a PR with something you think it needs.



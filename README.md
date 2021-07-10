# Luke's Programming Languages Zoo
Fine-grain (Small Step) implementations of common lambda calculi in Haskell.

## Motivation
I've been studying the Foundations of Programming Languages, Semantics, and Type Theory. I decided to implement some of the common Lambda Calculi to solidify my understanding. 

The naming of this repo was inspired in part by Andrej Bauer's [plzoo](https://github.com/andrejbauer/plzoo) but with a focus on the underlying calculus and semantics of functional languages.

One aim of the repo is to implement popular (functional) languages and extensions to portray how the theory translates into practice.

The languages are written in Haskell and are intentionally simple. That is, they do not use advanced features of Haskell but rather minimal use of [type constructors](https://en.wikipedia.org/wiki/Algebraic_data_type), [recursion](https://www.google.co.uk/search?ei=MbCyWrPmOeWWgAbLr4TwCg&q=recursion&oq=recursion&gs_l=psy-ab.3..35i39k1l2j0i67k1l8.6142.7685.0.7949.10.7.0.0.0.0.543.543.5-1.1.0....0...1c.1.64.psy-ab..9.1.541.0...0.9MwCQCbDFwA), and [functional programming](https://learnxinyminutes.com/docs/haskell/). 

The intention here is to maximise your understanding of language design whilst minimising the need to understand Haskell. Of course it helps if you know [it](http://learnyouahaskell.com/)!

See the [blog](https://lukegeeson.com/blog/2018-03-03-Lukes-Programming-Languages-Zoo/) for some more pointers and a fish!

## Languages

1. [_ULC_](ULC/): Alonzo Church's [Untyped Lambda Calculus](https://en.wikipedia.org/wiki/Lambda_calculus) (Church Style)
2. [_SKI_](SKI/): Moses Schonfinkel's [SKI Combinator Calculus](https://en.wikipedia.org/wiki/SKI_combinator_calculus). In essence an (untyped) combinator calculus equivalent in [computational power](https://en.wikipedia.org/wiki/Turing_completeness) to ULC, but without abstraction.
3. [_STLC_](STLC/): Alonzo Church's [Simply-Typed Lambda Calculus](https://en.wikipedia.org/wiki/Simply_typed_lambda_calculus) (Church) with one base type and function types
4. [_SystemT_](SystemT/): Kurt Godel's [System T](https://en.wikipedia.org/wiki/Dialectica_interpretation). In essence the STLC with [Nat](https://wiki.haskell.org/Peano_numbers) swapped out for the base type and [primitive recursion](https://www.quora.com/What-is-primitive-recursion) on Nats.
5. [_PCF_](PCF/): Gordon Plotkin's [Programming Computable Functions](http://www.cs.bham.ac.uk/~axj/pub/papers/Jung-2014-Teaching-denotational-semantics.pdf). In essence it's System T but using the [Y combinator](https://en.wikipedia.org/wiki/Fixed-point_combinator#Fixed_point_combinators_in_lambda_calculus) for general recursion instead of primitive.
6. [_Mu_](Mu/): Michel Parigot's [Lambda-Mu](https://www.cs.ru.nl/~freek/courses/tt-2011/papers/parigot.pdf). In essence it's STLC with [continuations](https://en.wikipedia.org/wiki/Continuation) that don't rely on the reduction strategy used.
7. [_SystemF_](SystemF/): John Reynolds' [System F](https://en.wikipedia.org/wiki/System_F). In essence it's STLC with [parametric polymorphism](https://en.wikipedia.org/wiki/Parametric_polymorphism) built in.
8. [_SOL_](SOL/): John Mitchell and Gordon Plotkin's SOL. In essence it's System F but with [existential types](https://medium.com/@stephenebly/an-introduction-to-existential-types-25c130ba61a4) made explicit.
9. [_Cata_](Cata/): In essence it's STLC with [inductive types](https://en.wikipedia.org/wiki/Inductive_type). 
10. [_Ana_](Ana/): In essence it's STLC with [coinductive types](https://en.wikipedia.org/wiki/Coinduction).
11. [_Sub_](Sub/): Benjamin Pierce's Lambda Calculus with Subtypes. In essence it's STLC with generalised records and [subtype polymorphism](https://en.wikipedia.org/wiki/Subtyping).
12. [_Omega_](Omega/): Renardel de Lavalette's [L(or λω)](https://core.ac.uk/download/pdf/82628447.pdf). In essence it's STLC with kinding and [type-operators](https://en.wikipedia.org/wiki/Type_constructor).
13. [_FOmega_](FOmega/): Jean Yves-Girard's [FOmega](https://en.wikipedia.org/wiki/Lambda_cube). In essence it's SystemF + Omega which enables higher-order polymorphism.
14. [_LF_](LF/): Bob Harper, Furio Honsell, and Gordon Plotkin's [Edinburgh Logical Framework](https://dl.acm.org/citation.cfm?id=138060). In essence it's STLC with pure first-order [dependent types](https://en.wikipedia.org/wiki/Dependent_type).
15. [_C_](C/): Thierry Coquand and Gerard Huet's [Calculus of Constructions](https://www.sciencedirect.com/science/article/pii/0890540188900053?via%3Dihub). In essence it is FOmega + LF written in a [pure type systems](https://ttic.uchicago.edu/~dreyer/course/papers/barendregt.pdf) style. This serves as the apex of the lambda cube and a [constructive](https://en.wikipedia.org/wiki/Constructivism_(philosophy_of_mathematics)) foundation of mathematics.

See each repo for details on installation/use.

## Contributions
Submit a PR if there's something you want to add or fix! Bearing in mind a few things:
1. Compile your code with `-W`, This catches any warnings. There shouldn't be any warnings 
2. Use [hlint](http://hackage.haskell.org/package/hlint), to handle code linting and suggestions. Like wall, there should be no suggesstions for file `Foo.hs` when running `hlint Foo.hs`.
3. Ensure code has 100% [Haddock](https://www.haskell.org/haddock/) coverage. This helps to document things if ever we want to.
4. All of this can be run automatically using Cabal (see below). Make sure to run these locally before you commit.
5. Keep in mind the motivations above, this code is not meant to be advanced Haskell, but rather simple (for demonstration) so try not to use advanced technologies if you can.

## Building

First make sure your cabal is up to date:
```
cabal update
```

Each language in the zoo can be built using cabal, or just using ghc in each directory. You can build a language from this directory using e.g:
```
cabal build ulc
```
and run it using:
```
cabal run ulc
```
and you will see something like:
```
⇒  cabal run ulc
Up to date
Welcome to the Untyped λ-calculus REPL
Type some terms or press Enter to leave.
>
```
You can build all of the languages with:
```
cabal build
```

Alternatively you can build each language with vanilla GHC. First by navigating into a language directory, you can do e.g:
```
⇒ cd ulc
⇒ ghc -O2 -o ulc Main -W
... Compilation bits ...
⇒ ./ulc
Welcome to the Untyped λ-calculus REPL
Type some terms or press Enter to leave.
>
```

## Testing

The languages in the zoo are tested using unit tests in the form of example terms, QuickCheck to test parsing of randomly generated terms. This is a work in progress but for the testsuites that exist you can use cabal to run the tests:
```
⇒ cabal test test-ulc
... Build bits ...
Test suite test-ulc: RUNNING...
+++ OK, passed 20 tests.
+++ OK, passed 20 tests.
Test suite test-ulc: PASS
Test suite logged to:
... Log dir ...
```
To run the tests for ulc (See the cabal file for the names of each testsuite). You can run all the testsuites at once with `cabal new-test`.

Alternatively you can use vanilla GHC to test each langauge (you'll need a local version of QuickCheck), using:
```
⇒  ghci Tests.hs
*Tests> runTests
+++ OK, passed 20 tests.
+++ OK, passed 20 tests.
```

See the Cabal file for the testsuites.

## Documentation

We use haddock to generate developer documentation for a particular language use:
```
⇒  cabal haddock stlc
... build things ...
Haddock coverage:
... checks all functions are covered ...
Documentation created:
... location where dev docs are stored ...
```
You can then open the `index.html` file in a browser to see the documentation


